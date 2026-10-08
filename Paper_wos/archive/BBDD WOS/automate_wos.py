#!/usr/bin/env python3
"""
WOS USA Yearly Paper Counter
━━━━━━━━━━━━━━━━━━━━━━━━━━━
Run:  python3 wos_usa_yearly.py
"""

import asyncio
import csv
import re
import sys
from datetime import datetime
from pathlib import Path
from playwright.async_api import async_playwright, TimeoutError as PWTimeout

# ── CONFIG ─────────────────────────────────────────────────────────────────────

WOS_URL = (
    "https://www-webofscience-com.pucdechile.idm.oclc.org"
    "/wos/woscc/advanced-search"
)

QUERY = """\
TS=("field experiment" OR "survey experiment" OR "audit stud" OR \
"correspondence experiment" OR "correspondence audit" OR \
"conjoint experiment" OR "conjoint analy" OR "list experiment" OR \
"factorial experiment" OR "factorial survey" OR "vignette experiment" OR \
"vignette survey") AND WC=("Education & Educational Research" OR \
"Economics" OR "Management" OR "Political Science" OR "Business" OR \
"Social Sciences, Interdisciplinary" OR "Communication" OR "Sociology" OR \
"Computer Science, Artificial Intelligence" OR \
"Computer Science, Interdisciplinary Applications" OR \
"International Relations" OR "Statistics & Probability" OR \
"Computer Science, Theory & Methods" OR "Health Policy & Services" OR \
"Social Work" OR "Public Administration" OR "Urban Studies" OR \
"Women's Studies" OR "Social Issues" OR \
"Social Sciences, Mathematical Methods" OR "Family Studies" OR \
"Development Studies" OR "Cultural Studies" OR "Demography" OR \
"Gerontology" OR "Social Sciences, Biomedical")
OR
TS=("field experiment" OR "survey experiment" OR "audit stud" OR \
"correspondence experiment" OR "correspondence audit" OR \
"conjoint experiment" OR "conjoint analy" OR "list experiment" OR \
"factorial experiment" OR "factorial survey" OR "vignette experiment" OR \
"vignette survey") AND WC=("Psychology, Multidisciplinary" OR \
"Psychology, Clinical" OR "Psychology, Applied" OR \
"Psychology, Experimental" OR "Psychology, Developmental" OR \
"Psychology" OR "Psychology, Social" OR "Psychology, Educational" OR \
"Psychology, Psychoanalysis" OR "Psychology, Biological" OR \
"Psychology, Mathematical")"""

OUTPUT_FILE = f"wos_usa_yearly_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv"
YEAR_RANGE  = range(1990, 2027)

# ── HELPERS ────────────────────────────────────────────────────────────────────

def hr():
    print("─" * 60)

def pause(msg: str):
    print()
    hr()
    for line in msg.strip().splitlines():
        print(f"  {line}")
    hr()
    input("  ↩  Press Enter when done...")
    print()

async def safe_wait(page, timeout=25000):
    try:
        await page.wait_for_load_state("networkidle", timeout=timeout)
    except PWTimeout:
        pass
    await page.wait_for_timeout(1200)

# ── STEP: Enter query ──────────────────────────────────────────────────────────

async def enter_query(page):
    print("⏳ Entering query...")
    try:
        box = page.locator('#advancedSearchInputArea').first
        await box.wait_for(state="visible", timeout=8000)
        await box.click()
        await page.keyboard.press("Control+a")
        await page.keyboard.press("Delete")
        await box.fill(QUERY)
        await page.wait_for_timeout(500)
        print("  ✓ Query entered")
        return
    except Exception as e:
        print(f"  Auto-fill failed ({e})")

    # Fallback: JS injection
    injected = await page.evaluate("""(query) => {
        const el = document.getElementById('advancedSearchInputArea')
                   || document.querySelector('textarea[name="search"]')
                   || document.querySelector('textarea');
        if (!el) return false;
        el.focus();
        const setter = Object.getOwnPropertyDescriptor(
            window.HTMLTextAreaElement.prototype, 'value').set;
        setter.call(el, query);
        el.dispatchEvent(new Event('input',  { bubbles: true }));
        el.dispatchEvent(new Event('change', { bubbles: true }));
        return true;
    }""", QUERY)

    if injected:
        await page.wait_for_timeout(500)
        print("  ✓ Query entered via JS")
        return

    print("\n  ⚠  Could not auto-enter query. Copy and paste it manually:\n")
    hr()
    print(QUERY)
    hr()
    pause("Paste the query into the WOS search box, then press Enter.")

# ── STEP: Click search ─────────────────────────────────────────────────────────

async def click_search(page):
    print("⏳ Clicking Search...")
    for sel in [
        'button[data-ta="run-search-button"]',
        'button[aria-label*="Search"]',
        'button:has-text("Search")',
    ]:
        try:
            btn = page.locator(sel).first
            if await btn.is_visible(timeout=4000):
                await btn.click()
                await safe_wait(page)
                print("  ✓ Search submitted")
                return
        except PWTimeout:
            continue
    pause("Could not auto-click Search. Please click it manually, wait for results, then press Enter.")
    await safe_wait(page)

# ── STEP: Expand year facet ────────────────────────────────────────────────────

async def expand_year_facet(page):
    print("⏳ Expanding Publication Years facet...")
    for sel in [
        '[data-ta="publication-year-facet"] button',
        'button[aria-label*="Publication Year"]',
        'button:has-text("Publication Year")',
        'span:has-text("Publication Year")',
    ]:
        try:
            el = page.locator(sel).first
            if await el.is_visible(timeout=3000):
                await el.click()
                await page.wait_for_timeout(1000)
                break
        except PWTimeout:
            continue

    # Click "Show More" to surface all years beyond the default ~10
    for _ in range(15):
        clicked = False
        for label in ["Show More", "Ver más", "More", "Show All"]:
            try:
                btn = page.locator(f'button:has-text("{label}")').first
                if await btn.is_visible(timeout=1500):
                    await btn.click()
                    await page.wait_for_timeout(800)
                    clicked = True
                    break
            except PWTimeout:
                continue
        if not clicked:
            break
    print("  ✓ Year facet expanded")

# ── STEP: Scrape year counts ───────────────────────────────────────────────────

async def scrape_year_counts(page) -> dict:
    """
    Extract {year: count} from the Publication Years checkboxes.
    WOS stores both values in aria-label: e.g. aria-label="2025, 4,804"
    Selector confirmed from browser inspector: input[name="PY"]
    """
    print("⏳ Scraping year counts from sidebar...")
    year_data = {}

    # Primary strategy: read aria-label from Publication Year checkboxes
    checkboxes = await page.locator('input[name="PY"]').all()
    print(f"  → Found {len(checkboxes)} year checkboxes")

    for cb in checkboxes:
        label = await cb.get_attribute("aria-label")
        if not label:
            continue
        # aria-label format: "2025, 4,804"
        m = re.match(r"((?:19|20)\d{2}),\s*([\d,]+)", label.strip())
        if m:
            yr  = int(m.group(1))
            ct  = int(m.group(2).replace(",", ""))
            if yr in YEAR_RANGE:
                year_data[yr] = ct

    if year_data:
        print(f"  ✓ Found {len(year_data)} years")
        return year_data

    print("  ✗ Could not extract year data automatically.")
    return year_data

# ── STEP: Manual fallback ──────────────────────────────────────────────────────

def manual_entry_fallback() -> dict:
    pause(
        "Auto-extraction failed.\n"
        "Look at the 'Publication Years' panel in WOS and type each row below.\n"
        "Format: YEAR COUNT  (e.g.  2023 1847)\n"
        "Enter a blank line when done."
    )
    year_data = {}
    print("Enter year data (YEAR COUNT), blank line to finish:")
    while True:
        line = input("  > ").strip()
        if not line:
            break
        parts = line.split()
        if len(parts) == 2 and parts[0].isdigit() and parts[1].replace(",", "").isdigit():
            year_data[int(parts[0])] = int(parts[1].replace(",", ""))
        else:
            print("    (skipped — expected: YEAR COUNT)")
    return year_data

# ── MAIN ───────────────────────────────────────────────────────────────────────

async def main():
    print()
    print("=" * 60)
    print("  WOS USA Yearly Paper Counter")
    print("=" * 60)

    async with async_playwright() as p:
        # Persistent profile: saves cookies/session between runs.
        # First run: log in manually. After that: already logged in.
        PROFILE_DIR = str(Path(__file__).parent / "wos_browser_profile")
        ctx  = await p.chromium.launch_persistent_context(
            user_data_dir = PROFILE_DIR,
            headless      = False,
            slow_mo       = 150,
            viewport      = {"width": 1440, "height": 900},
            args          = ["--disable-blink-features=AutomationControlled"],
        )
        page = await ctx.new_page()

        # 1. NAVIGATE TO WOS ───────────────────────────────────────────────────
        await page.goto(WOS_URL)
        pause("If prompted, log in to WOS through the PUC proxy.\n"
              "Once fully logged in, press Enter.")

        # Force navigation to advanced search (WOS sometimes redirects to history)
        print("⏳ Navigating to Advanced Search...")
        await page.goto(WOS_URL)
        await safe_wait(page)
        print("  ✓ On Advanced Search page")

        # 2. CLOSE ANY POPUPS ──────────────────────────────────────────────────
        # Remove the grey backdrop via JS so clicks are unblocked,
        # then let the user close the popup manually with the X button.
        await page.evaluate("""() => {
            document.querySelectorAll(
                '.cdk-overlay-backdrop, .onetrust-pc-dark-filter, ' +
                '[class*="backdrop"], [class*="overlay-bg"], ' +
                '[class*="modal-bg"], [class*="dark-filter"]'
            ).forEach(el => {
                el.style.pointerEvents = 'none';
                el.style.display = 'none';
            });
            document.body.style.overflow = 'auto';
        }""")
        await page.wait_for_timeout(500)
        pause("The grey overlay has been removed.\n"
              "Now click the X to close any popup on screen.\n"
              "Once the page is clear, press Enter.")

        # 3. ENTER QUERY ───────────────────────────────────────────────────────
        await enter_query(page)

        # 4. SEARCH ────────────────────────────────────────────────────────────
        await click_search(page)

        # 5. MANUAL FILTERS ────────────────────────────────────────────────────
        pause("Apply these filters in WOS:\n"
              "\n"
              "  [1] Publication Date: 1990-01-01  →  2026-06-01\n"
              "  [2] Document Type: Article  +  Early Access  (only these two)\n"
              "  [3] Countries/Regions: United States\n"
              "\n"
              "Wait for the result count to update, then press Enter.")
        await safe_wait(page)

        # 6. EXPAND YEAR FACET ─────────────────────────────────────────────────
        await expand_year_facet(page)

        # 7. SCRAPE YEARS ──────────────────────────────────────────────────────
        year_data = await scrape_year_counts(page)
        if not year_data:
            year_data = manual_entry_fallback()

        await ctx.close()

    # 8. OUTPUT ────────────────────────────────────────────────────────────────
    if not year_data:
        print("\n⚠  No data collected. Exiting.")
        sys.exit(1)

    sorted_rows = sorted(year_data.items())
    total = sum(v for _, v in sorted_rows)

    print()
    hr()
    print(f"  {'Year':<8}  {'Papers (USA)':>14}")
    hr()
    for yr, ct in sorted_rows:
        print(f"  {yr:<8}  {ct:>14,}")
    hr()
    print(f"  {'TOTAL':<8}  {total:>14,}")
    hr()

    with open(OUTPUT_FILE, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["year", "n_papers_usa"])
        writer.writerows(sorted_rows)

    print(f"\n✓ Saved → {OUTPUT_FILE}\n")


if __name__ == "__main__":
    asyncio.run(main())