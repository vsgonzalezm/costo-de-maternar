# Feasibility Assessment: Correspondence Audit Study of Hiring Discrimination in Chile
### Gender × continuous social-class signals

Prepared 14 August 2026. Sources and verification status noted throughout.

---

## 1. What I could and could not verify

**Verified directly:** Trabajando.com's public help/FAQ pages; Chilean Labour Code Art. 2 protected categories; BNE (Bolsa Nacional de Empleo) registration requirements; Ley 21.719 timeline; ENCLA 2023 content; regional audit-study designs (Peru, Argentina, Mexico, Ecuador); Laborum industry survey figures (via Emol/El Mercurio reporting).

**Could NOT verify directly:** Computrabajo and Laborum registration forms and terms of service — both sites are bot-blocked/client-rendered and returned empty to automated fetching. The Chrome browser extension was not connected during this session. **These require manual inspection before finalising the design** — see §7.

**Treat with caution:** Several statistics circulating in Chilean SEO/blog content (e.g. "87.1% of jobseekers use online job boards") could not be traced to a primary source and should not be cited. The Laborum figures below come from an industry actor with a commercial interest in the finding.

---

## 2. How Chilean firms actually recruit

### 2.1 The critical structural fact: two parallel channels

**ENADEL (Encuesta Nacional de Demanda Laboral, Subsecretaría del Trabajo/SENCE)** is the official survey of formal firms with 10+ employees and — unlike ENCLA — it *does* ask about recruitment channels. Reported findings indicate that the **most-cited recruitment mechanisms are worker recommendations and the employer's personal networks**, followed by OMIL (municipal labour offices) and the BNE. ENADEL 2024 also reports 68.5% of firms had or projected vacancies, and >60% reported hard-to-fill vacancies.

**This is the single most important design constraint.** Referral hiring dominates in exactly the segments where class discrimination is theoretically most likely (small firms, low-skill jobs, personalistic hiring). An online-platform audit measures discrimination *only in the formal, arms-length channel* — which is likely the **least** discriminatory part of the market. Bravo et al. (2008) anticipated precisely this in explaining their null result.

### 2.2 The digital channel is nonetheless large and growing

Laborum's *Reclutamiento y Postulación Digital* study (Nov 2024, reported in Emol): 80% of Chilean HR specialists use digital platforms as their primary talent-identification tool; 90% use digital tools for search and selection; 80% use them to publish vacancies; 40% cite automated filtering/time savings; 40% cite AI tools for profile identification. 93% of Chilean candidates search via digital platforms. *Caveat: Laborum is a job platform reporting on the value of job platforms.*

Practical implication: sector and firm-size stratification matters enormously. Expect online-channel coverage to be high in services, retail, finance, professional/technical and administrative roles, and low in agriculture and construction (where ENCLA 2023 shows 44–49% temporary contracts and where informal networks dominate).

### 2.3 ATS adoption creates heterogeneity in who/what screens

Large firms increasingly use Applicant Tracking Systems with automated filtering; SMEs often review CVs manually. This means your treatment may be read by an algorithm in some firms and a human in others — a real moderator worth measuring, and a threat to interpretation if unmeasured.

---

## 3. Platform-by-platform assessment

| Platform | Identity requirement | Feasibility | Notes |
|---|---|---|---|
| **BNE (Bolsa Nacional de Empleo)** | **RUT validated against Registro Civil; ClaveÚnica** | ❌ **Not usable** | State-operated; identity validated against government registries. Fictitious identities are both technically impossible and legally serious. Exclude. |
| **Trabajando.com** | **RUT collected and used as a unique account key** (verified: FAQ references "rut en uso" as an account problem and RUT-based account recovery). SMS/email 6-digit verification. | ⚠️ Constrained | RUT appears to be a uniqueness constraint, not a Registro Civil validation — but this must be confirmed. Rich outcome data available (see §5). |
| **Computrabajo** | Registration reportedly by email/Google/Apple; RUT requirement **unverified** | ⚠️ Likely most permissive | ToS reportedly prohibit fraudulent/deceptive activity and false information. Highest-volume board in Chile. Needs manual verification. |
| **Laborum** | Unverified | ⚠️ | ~2.6M monthly visits (unverified claim). Part of the Bumeran regional group. |
| **LinkedIn** | No RUT; real-name policy; strong anti-fake-account enforcement | ❌ Not recommended | Fake profiles are aggressively detected and removed; "Easy Apply" automation violates ToS. High detection and account-loss risk mid-fieldwork. |
| **Direct-to-employer email** | None | ✅ Viable supplement | Bravo et al. (2008) used physical mail, email and fax. Slower callback (18 days by mail vs. 8 by email in their data) but no platform intermediation. |

**Key finding on RUT:** the RUT constraint is *not* uniform. It is absolute on the state platform (Registro Civil validation), appears to be a uniqueness key on Trabajando.com, and may be absent at registration on Computrabajo. A valid-format RUT is trivially generable (the check digit is a published modulus-11 algorithm), but **generating check-digit-valid RUTs that may correspond to real people raises exactly the legal and ethical problem you must avoid** — see §6.

Note also that current Chilean CV convention is moving *away* from including RUT, birth date, marital status, photo, etc. on the CV itself. This helps: a CV without a RUT is now normal and not suspicious.

---

## 4. Semi-automation: what is realistic

**Realistic:** automated *vacancy scraping* to build the sampling frame (job ID, title, sector, firm size, region, posting date); automated CV/cover-letter generation from templates with randomised signal assignment; automated management of email inboxes and callback coding; automated randomisation and assignment logging.

**Not realistic without agreements:** fully automated submission at scale. Expect CAPTCHAs, bot-detection, rate limiting, SMS/email verification per account, and ToS clauses prohibiting automated access and false information. **You must not bypass CAPTCHAs or bot-detection.** Plan for human-in-the-loop submission by trained RAs — this is what every published LatAm audit study has done.

**Staffing implication:** Bravo et al. (2008) ran 8 CVs × ~69 vacancies/week with three category leads, three assistants, two supervisors, and a dedicated 24/7 two-person phone team with 8 separate mobile phones. Budget realistically.

---

## 5. Outcome measurement — an underexploited opportunity

Trabajando.com exposes a **granular application-status ladder** to candidates: *Postulación enviada → CV recibido → CV visto → CV evaluado → Proceso finalizado*.

This is a genuine methodological advance over binary callback. It lets you decompose the funnel:
- **CV visto** = did the recruiter open the application at all (attention/screening stage)
- **CV evaluado** = did they annotate/evaluate it (active consideration)
- **Callback** = contact

Discrimination could operate at any of these stages, and separating "never looked" from "looked and rejected" speaks directly to the statistical-vs-taste-based distinction. **Verify this status ladder is visible and reliably updated before building the design around it.** Combine with response *quality* coding (as in your Phase 2 plan): response latency, whether an interview is offered vs. a request for more information, tone/personalisation, and salary discussed.

---

## 6. Legal and ethical constraints — Chile-specific

### 6.1 Substantive law is on your side
Chilean Labour Code **Art. 2** explicitly lists **"situación socioeconómica," "origen social," "apariencia personal,"** and **"sexo"** among prohibited grounds of discrimination, and — critically — **expressly defines discriminatory job advertisements as acts of discrimination**. Your study tests conduct that Chilean law already prohibits. This is a strong justification framing and a strong policy-relevance argument.

### 6.2 Usurpación de nombre (Art. 214 Código Penal)
Using *another real person's* name/identity is a criminal offence, with aggravated penalties where done via the internet causing harm to third parties. **Design rule: fictitious identities must be verifiably non-existent persons** — never a real person's RUT-name combination. This argues strongly for avoiding RUT submission entirely wherever possible, and for platform selection driven by this constraint.

### 6.3 Ley 21.719 (data protection) — timing is critical
The new data protection law **enters into full force 1 December 2026**, replacing Ley 19.628, and creates a Data Protection Agency with fines up to 20,000 UTM. Your fieldwork will almost certainly fall *after* this date. Implications:
- You will be a data controller processing personal data of **recruiters** (names, emails, phone numbers, and inferred behaviour).
- Principles of lawfulness, purpose limitation, proportionality and security apply.
- Build a data management plan now: pseudonymise recruiter/firm identifiers at collection, store keys separately, define retention limits, and specify destruction of the fictitious-identity infrastructure at study close.
- **Get legal counsel on the research exemption/legitimate-interest basis under Ley 21.719 before fieldwork.** This is the single largest regulatory unknown and it is new enough that precedent is thin.

### 6.4 Deception and IRB
Standard correspondence-study ethics apply: no consent from employers, minimal burden justification, no interviews accepted (withdraw promptly and politely), no real applicants displaced. Riach & Rich (2004) is the standard citation for the ethics of deceptive field experiments. Pre-register the deception protocol, the withdrawal script, and the burden calculation. Expect your PUC ethics committee to focus on (a) recruiter time burden, (b) deception justification, (c) data protection under the new law.

---

## 7. Immediate verification checklist (before writing Phase 2)

1. **Manually create one candidate account** on Computrabajo, Laborum and Trabajando.com. Record: exact mandatory fields, whether RUT is required, whether RUT is validated beyond check-digit format, phone/SMS verification, whether one email/phone can hold multiple profiles.
2. **Read each platform's ToS in full** and document clauses on false information, automated access, and multiple accounts. Save dated copies.
3. **Confirm the Trabajando.com status ladder** is reliably populated (pilot with a handful of real applications).
4. **Pilot vacancy volume**: scrape 2–4 weeks of postings to establish weekly vacancy counts by sector, occupation and region. This determines whether your power analysis is achievable — this is the binding constraint on sample size.
5. **Establish baseline callback rate** in a pilot. Bravo et al. got 14.65% overall (12.1% professionals, 18.1% technicians, 14.2% unskilled); Weisshaar et al. and the LatAm studies provide comparison points. Your power analysis depends critically on this.
6. **Legal opinion** on Ley 21.719 compliance and on the fictitious-identity approach.

---

## 8. Potentialities (arguments for feasibility)

- **Regional precedent is strong and recent.** Multiple LatAm correspondence studies have run successfully on online platforms: Lima, Peru (4,820 CVs to 1,205 vacancies; sex and indigenous/white surnames; men +20% callbacks, whites +80% vs. indigenous); Buenos Aires (Zanoni, Acevedo & Hernández 2022, IDB — 4,290 *online* paired applications; slum residents −28% callbacks); Mexico City (Arceo-Gomez & Campos-Vázquez 2014, *AER* — **802 vacancies × 8 applications each, via an online portal**, with phenotype photographs); Ecuador (Venezuelan migrant penalty). The Mexico design in particular proves an 8-arm factorial is executable on a LatAm online job portal.
- **A factorial gender × class design is directly precedented**: Rivera & Tilcsik (2016, *ASR*) — class × gender signals, with the key finding that class and gender *interact* (higher-class men advantaged; higher-class women penalised via a "commitment" mechanism). This is the single closest design template for your hypotheses and validates the intersectional framing.
- **Class signalling has established, transportable operationalisations**: neighbourhood/comuna of residence (validated in Chile by Bravo et al.; in Argentina by Zanoni et al.; in Sweden by Carlsson & Eriksson), school attended (highly diagnostic in Chile given the municipal/subvencionado/particular pagado tripartite system), surname, extracurricular activities and leisure signals (Rivera & Tilcsik), scholarship mentions, and language/register in the cover letter.
- **Chile's institutional structure makes class signals unusually legible.** The school system, comuna segregation of Santiago, and surname-status associations are all strong, well-documented, and mutually reinforcing — which is exactly what a *continuous* multi-signal index needs.
- **Legal protection exists for the categories you are testing** (Art. 2), giving direct policy leverage.
- **Digital channel is now dominant enough** in formal-sector white-collar hiring to make a platform-based design representative of a meaningful market segment (unlike 2006, when *El Mercurio* print ads were the frame).
- **Van Borm, Lippens & Baert (2022)** provides a template for *controlling for social class while estimating ethnic stigma* — directly relevant to disentangling your origin and class dimensions.

---

## 9. Problems and risks

### 9.1 Design-critical
- **Referral channel invisibility.** Your estimate is a lower bound on total labour-market discrimination and is conditional on the formal application channel. Say so explicitly and repeatedly.
- **RUT/identity verification** may force you off the highest-volume platform. Resolve early (§7).
- **Detection risk rises steeply with arms per vacancy.** A full origin × class × gender factorial implies many CVs per posting. The literature's mitigations: stagger submissions over 4–7 days, vary CV formatting/templates/institutions across arms, cap arms per vacancy (consider a fractional factorial or randomising a subset of arms per posting), and audit most firms only once.
- **Power.** Chilean callback rates (~15%) plus a continuous class treatment plus interaction hypotheses is a demanding combination. Interactions require substantially more power than main effects — Rivera & Tilcsik found large effects in an elite market, but your effects may be smaller. Realistic power analysis may push you toward a large N (Bravo et al. managed 11,016 CVs; Weisshaar et al. 11,000+).
- **Continuous class measurement is a genuine methodological risk.** A multi-signal continuous index is a real advance, but: (a) signals may not be monotonic or additive; (b) recruiters may attend to one dominant cue (school) and ignore others, collapsing your continuum; (c) bundled signals create confounding with perceived competence/quality unless carefully validated. **This is precisely why Phase 1 exists** — but be prepared for the pretest to reveal that the index is better treated as ordinal categories, or that only a subset of signals move perceptions independently of perceived productivity.
- **Class signals may be read as competence signals.** School quality in Chile correlates with actual educational attainment. If recruiters read "particular pagado" as a proxy for skill rather than class, you are measuring statistical discrimination on productivity beliefs, not class animus. Your pretest must separately measure *perceived class* and *perceived competence/productivity* so you can hold the latter constant — this is the central validity threat.

### 9.2 Operational
- Platform ToS violations risk mid-fieldwork account termination and data loss. Mitigate with distributed accounts, multiple platforms, and conservative submission rates.
- CAPTCHAs, rate limiting and bot detection cap throughput; budget RA hours accordingly.
- Applications cannot be withdrawn on Trabajando.com once sent (verified) — so error correction is impossible. Pilot thoroughly.
- Callback capture requires dedicated phone/email infrastructure per identity (Bravo et al. used 8 dedicated mobile lines).
- Seasonality is substantial: Bravo et al. saw weekly callback rates range 6.15%–24.63%, with holiday weeks (Semana Santa) collapsing response. Block on calendar time.

### 9.3 Inferential
- **Callback ≠ hire.** You measure only the first funnel stage. Bravo et al. flagged this explicitly as a limit on their null.
- **Ceiling/floor and market-segment selection**: online-advertised jobs skew toward formal, urban, medium/large firms — precisely the firms ENCLA 2023 shows have *more* reported discrimination (25.4% in large firms vs. 4.5% in small), but also more formalised HR and anti-discrimination procedures. The direction of this bias is genuinely ambiguous; discuss rather than assume.
- **Heckman–Siegelman critique**: with a continuous treatment and multiple bundled signals, unobserved-variance differences across arms are a live concern. Consider the standard responses (matched-pair design, variance-controlled CV construction, and reporting sensitivity to the standardisation level).

---

## 10. Design recommendations

1. **Multi-platform, not single-platform.** Reduces ToS/termination risk and improves generalisability. Randomise platform or block on it.
2. **Exclude BNE entirely.** Registro Civil validation makes it both infeasible and legally inadvisable.
3. **Build the class index from signals that are (a) independently pretested, (b) plausibly orthogonal to productivity, and (c) individually manipulable** — favour comuna, school, extracurriculars/leisure, and cover-letter register over anything that reads as credential quality.
4. **Measure perceived competence alongside perceived class in Phase 1**, and select signals that move class perception *without* moving competence perception. This is the pivotal validation criterion.
5. **Exploit the status ladder** (CV visto / CV evaluado) as intermediate outcomes if verified — this is a distinctive contribution and strengthens mechanism claims.
6. **Stratify and pre-register by occupation and sector**, prioritising sectors with high online-channel penetration and adequate vacancy volume.
7. **Pre-register the deception protocol, withdrawal script, burden calculation, and data-protection plan** — and cite Ley 21.719 compliance explicitly. In a post-Dec-2026 environment this will strengthen, not weaken, the proposal.
8. **Frame the estimate as a lower bound on formal-channel discrimination**, and consider pairing with a complementary design (e.g. a recruiter survey experiment or vignette study) that can reach the referral channel indirectly.

---

## Key references to pull

- Bravo, Sanhueza & Urzúa (2008), IADB WP R-541 — the only Chilean labour audit study.
- Rivera & Tilcsik (2016), *ASR* 81(6):1097–1131 — class × gender factorial; closest template.
- Weisshaar, Chavez & Hutt (2024), *ASR* — design base.
- Zanoni, Acevedo & Hernández (2022), IDB WP 12122 — online paired design, Argentina, neighbourhood signal.
- Arceo-Gomez & Campos-Vázquez (2014), *AER* 104(5):376–380 — 8 arms per vacancy on a LatAm online portal.
- Van Borm, Lippens & Baert (2022), IZA DP 15707 — ethnic stigma controlling for social class.
- Gaddis (2017a, 2017b; 2018) — signal validation; the Phase 1 justification.
- Neumark (2018), *JEL* 56(3) — Heckman–Siegelman critique and responses.
- Baert (2017), IZA DP 10738 — near-complete correspondence-experiment inventory since 2005.
- Riach & Rich (2004), *Kyklos* 57(3) — ethics of deceptive field experiments.
