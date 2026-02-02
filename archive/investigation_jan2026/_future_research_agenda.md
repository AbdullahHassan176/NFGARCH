# Future Research Agenda: When Does NF-GARCH Work?

## Current Findings (What We Know)

From your dissertation, we've established:
1. ✅ **NF-GARCH works for fat-tailed distributions** (sGARCH_sstd, gjrGARCH: +0.3-1.2% MSE)
2. ❌ **NF-GARCH fails for Gaussian distributions** (sGARCH_norm: -2% MSE)
3. 🎯 **Distributional compatibility is key** - NF residuals must align with GARCH dynamics
4. 📊 **Improvements are modest** (0.3-1.2%) but statistically significant where they work
5. 🌍 **Asset-class matters** - Different volatility regimes show different sensitivities

---

## Research Extensions: Priority Order

### 🔴 **HIGH PRIORITY: Core Mechanism Questions**

These directly address "when NF-GARCH works" and are most publishable:

---

#### **1. Systematic Distribution Testing** 
**Goal:** Identify which innovation distributions benefit from NF enhancement

**Research Question:** 
> "How does NF-GARCH performance vary across different innovation distributions, and what distribution characteristics predict NF effectiveness?"

**Methodology:**
- Test NF-GARCH with ALL standard GARCH distributions:
  - ✅ Normal (done - fails)
  - ✅ Student-t (done - works)
  - ✅ Skewed-student-t (done - works best)
  - 🆕 GED (Generalized Error Distribution)
  - 🆕 Johnson's SU
  - 🆕 Skewed-GED
  - 🆕 Normal Inverse Gaussian (NIG)
  - 🆕 Variance Gamma (VG)

**Analysis:**
- For each distribution, measure:
  - NF improvement vs Standard GARCH
  - Residual kurtosis, skewness
  - Tail heaviness (Hill estimator)
  - Compatibility score (KL divergence between NF output and assumed distribution)

**Expected Outputs:**
- Chart: "NF Improvement vs Excess Kurtosis"
- Table: "Distribution Compatibility Matrix"
- Decision tree: "When to use NF-GARCH based on distributional diagnostics"

**Timeline:** 1-2 months (master's thesis extension)  
**Publishability:** High - Core theoretical contribution

---

#### **2. Model Complexity Analysis**
**Goal:** Determine optimal NF architecture complexity for different scenarios

**Research Question:**
> "What is the optimal NF architecture complexity (layers, hidden units) as a function of data characteristics (sample size, volatility, tail heaviness)?"

**Methodology:**
```r
# Test NF architectures:
architectures <- list(
  simple = list(n_layers = 2, hidden_dim = 32),
  medium = list(n_layers = 4, hidden_dim = 64),  # Current
  complex = list(n_layers = 8, hidden_dim = 128),
  very_complex = list(n_layers = 12, hidden_dim = 256)
)

# Vary sample sizes:
sample_sizes <- c(500, 1000, 2500, 5000)  # Current: 2934

# Analysis:
for each (architecture, sample_size):
  - Train NF on residuals
  - Measure overfitting (train vs test MSE gap)
  - Measure forecast improvement
  - Calculate complexity penalty (parameters / sample size)
```

**Key Metrics:**
- **Parsimony score:** Improvement / (parameters + 1)
- **Overfitting index:** (MSE_test - MSE_train) / MSE_train
- **Optimal complexity curve:** Best architecture vs sample size

**Expected Finding:**
- Small samples (n<1000): Simple NF (2 layers)
- Medium samples (1000-3000): Medium NF (4 layers) ← Your current case
- Large samples (>5000): Can support complex NF

**Timeline:** 2-3 months  
**Publishability:** Medium-High - Practical guidance

---

#### **3. Regime-Dependent Performance**
**Goal:** Understand if NF-GARCH works better in specific market conditions

**Research Question:**
> "Does NF-GARCH performance vary across volatility regimes (calm vs crisis), and can we predict when to switch between Standard and NF-GARCH?"

**Methodology:**
- Identify regimes using:
  - Markov-Switching GARCH
  - Rolling volatility windows
  - Crisis dummy variables (2008 GFC, 2020 COVID)

- Compare NF vs Standard GARCH:
  - **Low volatility regime** (σ < 1%)
  - **Medium volatility regime** (1% < σ < 3%)
  - **High volatility regime** (σ > 3%)

**Analysis:**
```r
# For each regime:
regimes <- c("low_vol", "medium_vol", "high_vol", "crisis")

for(regime in regimes):
  - Fit both Standard and NF-GARCH
  - Measure MSE, VaR coverage
  - Calculate regime-specific win rate
  
# Regime-switching model:
switch_rule <- if(current_vol > threshold):
                 use NF-GARCH
               else:
                 use Standard GARCH
```

**Expected Finding:**
- **Crisis periods:** NF-GARCH excels (captures extreme tail behavior)
- **Calm periods:** Standard GARCH sufficient (less noise)
- **Optimal switching:** Dynamic regime-based model selection

**Timeline:** 2-3 months  
**Publishability:** High - Practical + theoretical value

---

### 🟡 **MEDIUM PRIORITY: Asset & Data Characteristics**

These explore boundary conditions and generalizability:

---

#### **4. Asset Class Expansion**
**Goal:** Test if findings generalize beyond equity/FX

**New Asset Classes to Test:**
- **Fixed Income:** Government bonds, corporate bonds
- **Commodities:** Oil, gold, agricultural products
- **Cryptocurrencies:** Bitcoin, Ethereum (extreme volatility)
- **Volatility Indices:** VIX, VXN
- **Emerging Markets:** Higher volatility, different distributions

**Research Question:**
> "How does NF-GARCH performance vary across asset classes with different volatility characteristics?"

**Hypothesis:**
- Crypto (highest vol): Largest NF benefit
- Fixed income (lowest vol): Minimal NF benefit
- Commodities (fat tails): Moderate NF benefit

**Timeline:** 1-2 months (data collection + analysis)  
**Publishability:** Medium - Empirical extension

---

#### **5. Frequency & Horizon Analysis**
**Goal:** Determine optimal data frequency and forecast horizon for NF-GARCH

**Dimensions to Vary:**
- **Data Frequency:**
  - High-frequency: 5-min, 1-hour
  - Daily (current)
  - Weekly
  - Monthly

- **Forecast Horizon:**
  - 1-step ahead
  - 5-step ahead
  - 20-step ahead (current)
  - 60-step ahead

**Research Question:**
> "At which data frequencies and forecast horizons does NF-GARCH provide the largest improvements?"

**Expected Findings:**
- **High-frequency data:** NF captures intraday patterns, larger benefit
- **Long horizons:** Standard GARCH converges to unconditional mean, NF less useful
- **Sweet spot:** Daily data, 5-20 step horizons

**Timeline:** 1-2 months  
**Publishability:** Medium

---

#### **6. Cross-Asset Dependence**
**Goal:** Extend NF-GARCH to multivariate setting

**Research Question:**
> "Can NF capture cross-asset dependencies better than multivariate GARCH (DCC, BEKK)?"

**Methodology:**
- Implement **Multivariate NF-GARCH:**
  - Fit DCC-GARCH or BEKK-GARCH
  - Extract standardized residuals (multivariate)
  - Train NF on joint distribution (captures tail dependence, asymmetry)
  - Generate synthetic multivariate residuals
  - Forecast covariance matrix

**Applications:**
- Portfolio optimization
- Risk parity strategies
- Copula-based risk models

**Timeline:** 3-6 months (more complex)  
**Publishability:** High - Novel contribution

---

### 🟢 **LOWER PRIORITY: Methodological Refinements**

These are "nice to have" but less critical for understanding when NF works:

---

#### **7. Alternative NF Architectures**
**Goal:** Test if different NF types outperform your current approach

**Architectures to Compare:**
- ✅ Coupling flows (RealNVP) - Current
- 🆕 Autoregressive flows (MAF, IAF)
- 🆕 Continuous normalizing flows (Neural ODEs)
- 🆕 Residual flows
- 🆕 Spline-based flows

**Timeline:** 2-3 months  
**Publishability:** Medium (technical paper)

---

#### **8. Online/Adaptive NF**
**Goal:** Allow NF to adapt over time as distribution shifts

**Research Question:**
> "Can we update NF parameters online as new data arrives, rather than re-training from scratch?"

**Methodology:**
- Implement **rolling window NF training:**
  - Initial training: First 2000 observations
  - Update: Re-train every 250 days on rolling 2000-day window
  - Compare: Static NF vs Adaptive NF

**Timeline:** 2-3 months  
**Publishability:** Medium

---

#### **9. Hybrid Approaches**
**Goal:** Combine NF with other enhancement techniques

**Combinations to Test:**
- **NF + LSTM:** NF for distribution, LSTM for temporal dynamics
- **NF + HAR:** Realized volatility (HAR) + NF innovations
- **NF + Jump Models:** Separate jump component from NF-enhanced diffusion
- **Ensemble:** Average NF-GARCH and Standard GARCH forecasts

**Timeline:** 2-4 months  
**Publishability:** Medium-High if ensemble shows improvement

---

## Recommended Research Path (Timeline)

### **Phase 1: Master's Thesis Extension (3-6 months)**
**Goal:** Deepen core contribution - "When NF-GARCH Works"

1. **Month 1-2:** Systematic Distribution Testing (#1)
   - Test GED, Skewed-GED, Johnson's SU
   - Create compatibility matrix
   - Publish: "Distributional Compatibility in NF-GARCH Models"

2. **Month 3-4:** Regime-Dependent Performance (#3)
   - Identify volatility regimes
   - Test regime-specific performance
   - Publish: "Adaptive Model Selection for NF-GARCH"

3. **Month 5-6:** Write up + Submit
   - Target: Journal of Financial Econometrics, Quantitative Finance
   - Story: "NF-GARCH as a Conditional Enhancement"

### **Phase 2: PhD Expansion (1-2 years)** 
**If pursuing PhD:**

**Year 1:**
- Cross-Asset Dependence (#6) - Multivariate NF-GARCH
- Asset Class Expansion (#4) - Crypto, commodities, bonds
- Frequency Analysis (#5) - High-frequency applications

**Year 2:**
- Hybrid Approaches (#9) - Ensemble methods
- Real-world applications - Portfolio optimization, risk management
- Consolidate into PhD thesis

**Target Publications:**
- 2-3 top-tier journal articles (JF, JFE, RFS)
- 1 computational finance conference paper

---

## Immediate Next Steps (This Month)

If you want to extend immediately:

### **Quick Win: Asset Class Analysis** (2 weeks)
**Goal:** Test if equity vs FX finding holds for more assets

**Data to Add:**
- Cryptocurrencies: BTC, ETH (high vol)
- Commodities: Gold (GLD), Oil (USO)
- Bonds: TLT (low vol)

**Analysis:**
- Rerun full pipeline with expanded asset set
- Check if pattern holds: High vol assets benefit more from NF

**Output:** Extended results table showing NF benefit vs asset volatility

---

### **Quick Win: Kurtosis Threshold** (1 week)
**Goal:** Find empirical threshold for when NF helps

**Analysis:**
```r
# For each asset-model combination:
residual_kurtosis <- measure_kurtosis(standardized_residuals)
nf_improvement <- (MSE_standard - MSE_nf) / MSE_standard

# Fit regression:
model <- lm(nf_improvement ~ residual_kurtosis + I(residual_kurtosis^2))

# Find threshold:
kurt_threshold <- find_zero_crossing(model)
```

**Expected Output:**
> "NF-GARCH provides improvement when excess kurtosis > 5.2"

**Use:** Practical decision rule for when to use NF

---

## Tools & Resources Needed

### **Software:**
- ✅ R (rugarch, torch) - Already set up
- 🆕 Python (statsmodels, arch) - For alternative implementations
- 🆕 Julia (DifferentialEquations.jl) - For continuous normalizing flows

### **Data:**
- ✅ Equity + FX (current)
- 🆕 Crypto APIs: CoinGecko, Binance
- 🆕 Fixed income: FRED, Bloomberg
- 🆕 High-frequency: IEX Cloud, Polygon.io

### **Computational Resources:**
- Current: Local machine (sufficient for daily data)
- For HF data: Cloud computing (AWS, GCP) or university HPC cluster

---

## Publication Strategy

### **Target Venues:**

**Tier 1 (Top Finance):**
- Journal of Finance
- Journal of Financial Economics  
- Review of Financial Studies
- Journal of Econometrics

**Tier 2 (Specialized):**
- Journal of Financial Econometrics ⭐ (Best fit!)
- Quantitative Finance
- Journal of Banking & Finance
- Journal of Empirical Finance

**Tier 3 (Computational):**
- Computational Economics
- International Journal of Forecasting
- Applied Economics

### **Conference Presentations:**
- Western Finance Association (WFA)
- European Finance Association (EFA)
- SoFiE (Society for Financial Econometrics) ⭐
- Econometric Society meetings

---

## Collaboration Opportunities

Consider reaching out to:

### **Academic Collaborators:**
- **Volatility modeling experts:** Engle, Hansen, Patton
- **ML in finance:** Dixon, Hens, López de Prado
- **Normalizing flows:** Papamakarios, Kobyzev

### **Industry Partners:**
- Quant hedge funds (Two Sigma, Renaissance, Citadel)
- Risk management firms (MSCI, Axioma)
- Financial ML platforms (Numerai, Quantopian successors)

**Benefits:**
- Access to proprietary data
- Real-world validation
- Potential job opportunities

---

## Practical Recommendations

### **For Master's Thesis Extension (Next 6 Months):**

**Priority 1:** Systematic Distribution Testing (#1)
- Answers core question: "When does NF work?"
- Publishable in good journal
- Builds on your existing framework

**Priority 2:** Regime Analysis (#3)
- Practical value
- Shows when to dynamically switch models
- Adds real-world applicability

**Skip for now:**
- Alternative architectures (technical, less impactful)
- Online adaptation (complex, incremental value)

### **For PhD Pursuit (2+ Years):**

**Core Thesis:** "Distributional Learning in Volatility Forecasting"
- Chapter 1: NF-GARCH foundations (your current work)
- Chapter 2: Multivariate NF-GARCH (#6)
- Chapter 3: Regime-dependent model selection (#3)
- Chapter 4: Applications (portfolio optimization, risk management)

### **For Industry Career:**

**Focus on:**
- Practical implementation (#3 Regime analysis)
- Asset class expansion (#4) - especially crypto
- Real-time performance
- Production code quality

**De-emphasize:**
- Pure theory
- Novel architectures
- Academic publication

---

## Key Questions to Answer

Your research should ultimately address:

1. ✅ **When does NF-GARCH work?** → Fat-tailed distributions (Done)
2. ⏳ **Why does it work?** → Distributional compatibility (Partially answered)
3. ❓ **How much data is needed?** → Sample size requirements
4. ❓ **Which NF architecture?** → Complexity vs sample size trade-off
5. ❓ **Can we predict effectiveness?** → Pre-deployment diagnostics
6. ❓ **Dynamic switching?** → Regime-dependent model selection
7. ❓ **Multivariate extension?** → Cross-asset dependencies
8. ❓ **Real-world impact?** → Portfolio returns, risk metrics

**Target:** Answer questions 3-6 in master's extension, 7-8 in PhD.

---

## Success Metrics

### **Academic Success:**
- ✅ 1 top-tier publication (JFE, Quantitative Finance)
- ✅ 100+ citations within 5 years
- ✅ Invited to present at major conferences
- ✅ Method adopted in textbooks/courses

### **Industry Success:**
- ✅ Implementation by major financial institutions
- ✅ Open-source library with 1000+ users
- ✅ Consulting opportunities
- ✅ Quant job offers at top firms

### **Personal Success:**
- ✅ Deep understanding of when/why NF works
- ✅ Strong computational skills
- ✅ Network in quant finance community
- ✅ Career flexibility (academia or industry)

---

## Final Recommendation

**Best Path Forward (Next 6-12 Months):**

1. **Immediate (1-2 months):**
   - Add cryptocurrencies to test high-volatility regime
   - Calculate kurtosis threshold for NF effectiveness
   - Write up current findings as working paper

2. **Short-term (3-4 months):**
   - Systematic distribution testing (GED, Skewed-GED)
   - Regime-dependent analysis (crisis vs calm)
   - Submit to Journal of Financial Econometrics

3. **Medium-term (5-12 months):**
   - Wait for reviews, revise paper
   - Start multivariate extension if pursuing PhD
   - Present at conferences (SoFiE, WFA)
   - Apply for PhD programs (if interested)

**Key Message:** Focus on **WHEN** NF works (distributional compatibility, regime dependence) rather than incremental technical improvements. This provides maximum academic and practical value.

---

## Questions to Consider

Before starting extensions:

1. **Career goal:** Academia or industry?
   - Academia → Focus on #1, #3, #6 (theory + novelty)
   - Industry → Focus on #3, #4, #5 (practical applications)

2. **Time horizon:** 6 months or 2+ years?
   - 6 months → Master's extension (#1, #3)
   - 2+ years → PhD (#1-#9 comprehensive)

3. **Resources:** Solo or with collaborators?
   - Solo → Focus on extensions of current work
   - Collaborators → Multivariate/hybrid approaches

4. **Passion:** Theory or applications?
   - Theory → Distribution testing, why NF works
   - Applications → Portfolio optimization, risk management

**Let me know your preferences and I can create a detailed 3-6 month research plan!**
