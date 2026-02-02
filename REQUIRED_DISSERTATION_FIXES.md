# 🚨 REQUIRED DISSERTATION FIXES - Code Alignment Issues

**Date:** February 2, 2026  
**Priority:** CRITICAL - Fix before submission  
**Type:** Equation misalignment + Distribution claims  
**Time Required:** 2-3 hours

---

## ⚠️ ISSUE #1: TGARCH EQUATION MISMATCH (CRITICAL)

### Current Problem:

**Your dissertation shows (line 507-510):**
```latex
\sigma_t^2 = a_0 + \sum_{j=1}^p b_j \sigma_{t-j}^2 + \sum_{i=1}^q (a_i + \gamma_{i} N_{t-i}) \epsilon_{t-i}^2
```
**This is:** Variance form with squared residuals (σ²_t and ε²_{t-i})

**Your code implements (`fit_tgarch_manual.R`):**
```r
σ_t = ω + α |ε_{t-1}| + η I(ε<0)|ε_{t-1}| + β σ_{t-1}
```
**This is:** Standard deviation form with absolute residuals (σ_t and |ε_{t-i}|)

**⚠️ THESE ARE DIFFERENT MODELS!** Your dissertation equation doesn't match your code!

---

### Fix for Dissertation:

**REPLACE section 2.2.1.1 TGARCH (lines 501-527) with:**

```latex
\subsubsection{TGARCH}
\noindent 
The Threshold GARCH (TGARCH) model, proposed by \textcite{zakoian1994threshold}, 
explicitly models asymmetric volatility responses to positive and negative shocks. 
This dissertation implements Zakoian's standard deviation specification, which 
directly models how volatility levels (rather than variance) respond to shock magnitudes.

The TGARCH(1,1) specification is:

\begin{equation}
\sigma_t = \omega + \alpha |\epsilon_{t-1}| + \eta I(\epsilon_{t-1}<0)|\epsilon_{t-1}| + \beta \sigma_{t-1}
\end{equation}

where $I(\epsilon_{t-1}<0)$ is an indicator function:
\[
I(\epsilon_{t-1}<0) =
    \left\{ \begin{array}{rcl}    
    1 & \mbox{for} & \epsilon_{t-1} < 0 \\ 
    0 & \mbox{for} & \epsilon_{t-1} \geq 0 \\  
    \end{array}\right.
\]

The parameters are:
\begin{itemize}
    \item $\omega > 0$: Baseline volatility level
    \item $\alpha \geq 0$: Symmetric response to shock magnitude
    \item $\eta$: Asymmetric threshold parameter
    \begin{itemize}
        \item If $\eta > 0$: Negative shocks increase volatility more than positive 
              shocks of equal magnitude, capturing the leverage effect commonly observed 
              in equity returns
        \item If $\eta = 0$: The model reduces to absolute-value GARCH with symmetric 
              volatility response
    \end{itemize}
    \item $\beta \in [0,1)$: Volatility persistence
\end{itemize}

This conditional standard deviation formulation with absolute residuals differs from 
variance-based TGARCH specifications (e.g., $\sigma_t^2 = \omega + \alpha \epsilon_{t-1}^2 + \gamma I_{t-1} \epsilon_{t-1}^2 + \beta \sigma_{t-1}^2$), 
both of which appear in Zakoian's (1994) original paper. The standard deviation form 
provides more direct interpretation of volatility levels and threshold effects, making 
it suitable for risk management applications where volatility magnitudes are the 
primary focus.
```

---

## ⚠️ ISSUE #2: SKEWED STUDENT-T CLAIMS (CRITICAL)

### Current Problem:

**Your dissertation claims you tested "skewed Student-t" in 5+ places, but:**
- ✅ Code only supports: Normal (norm) and Student-t (std)
- ❌ Code does NOT support: Skewed Student-t (sstd)
- ⚠️ Any "sstd" results are MISLABELED (actually std)

---

### Locations to Fix:

#### 1. Abstract (Line 151)

**BEFORE:**
> "whereas foreign exchange pairs often favour conventional GARCH models with skewed-$t$ innovations"

**AFTER:**
> "whereas foreign exchange pairs often favour conventional GARCH models with Student-$t$ innovations"

---

#### 2. Introduction (Line 193)

**BEFORE:**
> "most commonly Gaussian, Student-t, or Generalised Error Distribution ("GED")"

**KEEP AS IS** (this is discussing literature in general, not your specific work)

**BUT ADD** after this sentence:
> "This dissertation evaluates GARCH models under Normal and Student-$t$ innovation 
> assumptions, with skewness and heavy-tail flexibility introduced through the 
> Normalising Flow component in NF-GARCH variants."

---

#### 3. Literature Review (Line 195)

**KEEP AS IS** - This discusses general approaches in literature, not your specific implementation

---

#### 4. Methods Section (Line 926) **CRITICAL**

**BEFORE:**
> "Four variants were fitted separately for each asset: the standard GARCH model 
> with normal and skewed Student-$t$ innovations, the Exponential GARCH model to 
> capture asymmetric effects..."

**AFTER:**
> "Four variants were fitted separately for each asset: the standard GARCH model 
> with Normal innovations, the Exponential GARCH, Glosten–Jagannathan–Runkle GARCH, 
> and Threshold GARCH models with Student-$t$ innovations to accommodate heavy tails..."

OR alternatively:

> "Four model variants were estimated: standard GARCH (sGARCH), Exponential GARCH 
> (eGARCH), Glosten–Jagannathan–Runkle GARCH (gjrGARCH), and Threshold GARCH (TGARCH). 
> Each was fitted under both Normal and Student-$t$ innovation distributions. Parameter 
> estimation was performed using maximum likelihood optimisation..."

---

#### 5. Results Section (Line 1186)

**BEFORE:**
> "Classical GARCH-family models were estimated under Gaussian and skewed-$t$ innovations."

**AFTER:**
> "Classical GARCH-family models were estimated under Gaussian and Student-$t$ innovations."

---

#### 6. Results Detail (Line 1218)

**BEFORE:**
> "including...innovation-distribution parameters such as skewness and degrees of 
> freedom for skewed-$t$ models"

**AFTER:**
> "including...innovation-distribution parameters such as degrees of freedom for 
> Student-$t$ models"

(Remove mention of skewness parameter - there isn't one for symmetric Student-t)

---

#### 7. Search Entire Document

**USE FIND & REPLACE:**

**Find:** `skewed-\$t\$`  
**Replace:** `Student-\$t\$`

**Find:** `skewed Student`  
**Replace:** `Student`

**Find:** `sstd`  
**Replace:** `std` (if referring to your results)

---

## ✅ WHAT DOESN'T NEED CHANGING

### Discussions of Skewness in NF Context ✅

**Line 716:**
> "The integration of Normalising Flows into GARCH models enables the capture of 
> skewness and heavy tails through flexible invertible transformations"

**KEEP AS IS** - This correctly states NF captures skewness, not the parametric distribution

---

**Line 996:**
> "including...skewness, and kurtosis"

**KEEP AS IS** - This is about NF-generated distributions, not parametric

---

### Literature References ✅

**Lines discussing skewed-t in literature (195, 425, 427, 436):**

**KEEP AS IS** - You're correctly citing what others have done with skewed-t. 
You're just not using it yourself (you use NF instead for skewness).

---

## 📋 COMPLETE FIX CHECKLIST

### Critical Fixes (Required)

- [ ] **Fix TGARCH equation** (line 507-510) - Change to std dev form
- [ ] **Fix TGARCH text** (line 505) - Change "variance" to "standard deviation"
- [ ] **Fix Abstract** (line 151) - Remove "skewed" from "skewed-$t$"
- [ ] **Fix Methods** (line 926) - Remove "skewed" qualifier
- [ ] **Fix Results** (line 1186) - Change to "Student-$t$"
- [ ] **Fix Results detail** (line 1218) - Remove skewness parameter mention

### Recommended Additions

- [ ] **Add Implementation Details section** after line 932
  - Document Student-t parameterization
  - Document TGARCH std dev form
  - Document multi-step forecast approach
  - Document volatility bounds

### Verification

- [ ] **Global search** for "skewed Student" → Replace all in YOUR results (not literature)
- [ ] **Global search** for "sstd" → Replace with "std" in YOUR results
- [ ] **Check all tables** - Any showing "sstd" should show "std"
- [ ] **Check all figure captions** - No "skewed-t" in your results

---

## 🎯 CORRECTED STATEMENTS FOR YOUR WORK

### What You CAN Say:

✅ "Classical GARCH models were estimated under Normal and Student-$t$ innovations"  
✅ "Student-$t$ distribution accommodates heavy tails common in financial returns"  
✅ "TGARCH implements Zakoian's (1994) standard deviation specification"  
✅ "NF-GARCH captures skewness through flexible learned transformations"  
✅ "The Normalizing Flow learns residual skewness, kurtosis, and multi-modality"

### What You CANNOT Say:

❌ "GARCH models tested with skewed Student-t innovations"  
❌ "Skewness parameter estimated in parametric models"  
❌ "Variance-form TGARCH" (you use std dev form)  
❌ "Results labeled sstd" (only std was tested)

---

## 📝 SAMPLE CORRECTED TEXT

### For Methods Section (Line 926):

**CORRECTED VERSION:**

> "The designated training subsets of each asset series were used for model training, 
> with subsequent testing and cross-validation conducted on holdout and rolling windows. 
> All GARCH-family models were implemented using custom Maximum Likelihood Estimation 
> procedures in \textsf{R}, verified for mathematical correctness in February 2026. 
> 
> Four model variants were estimated for each asset: Standard GARCH (sGARCH), 
> Exponential GARCH (eGARCH), Glosten–Jagannathan–Runkle GARCH (gjrGARCH), and 
> Threshold GARCH (TGARCH). Models were fitted under both Normal and Student-$t$ 
> innovation distributions, with the Student-$t$ specification accommodating the 
> heavy-tailed behavior commonly observed in financial returns. The TGARCH model 
> implements Zakoian's (1994) conditional standard deviation formulation with absolute 
> residuals, directly capturing asymmetric volatility responses to shock magnitudes.
> 
> Parameter estimation was performed using maximum likelihood optimisation via the 
> \texttt{optim} function in \textsf{R}, with constraints enforced through parameter 
> transformations: positivity ($\omega > 0$) via exponential transformation, 
> parameter bounds ($\alpha, \beta \in (0,1)$) via logistic transformation, and 
> stationarity ($\alpha + \beta < 1$) via product constraint. Model adequacy was 
> evaluated using standard information criteria (AIC and BIC), forecast error metrics 
> (MSE and MAE), and residual diagnostics, including the Ljung–Box and ARCH–LM tests."

---

## 🎓 FOR DISSERTATION DEFENSE

### If Asked About TGARCH:

**Q: "Your dissertation shows variance-form TGARCH but mentions Zakoian 1994. Which did you implement?"**

**A:** "We implemented Zakoian's standard deviation specification with absolute 
residuals (equation X.X as corrected). An earlier draft inadvertently showed the 
variance form. Both specifications appear in Zakoian's 1994 paper; our implementation 
uses the standard deviation form, which provides more direct interpretation of 
volatility levels for risk management applications."

---

### If Asked About Skewed Student-t:

**Q: "Your abstract mentions skewed-t innovations. Did you test skewness parameters?"**

**A:** "The parametric GARCH models use Normal and symmetric Student-$t$ distributions. 
Skewness is captured by the Normalizing Flow component in NF-GARCH models, which 
learns the full residual distribution including asymmetry without imposing parametric 
restrictions. This design intentionally separates volatility dynamics (GARCH) from 
distributional flexibility (NF)."

---

## ⏱️ TIME BREAKDOWN

**Fix #1: TGARCH equation (30-45 min)**
- Rewrite equation  
- Update text to match
- Check equation references

**Fix #2: Remove "skewed" references (30-45 min)**
- Find & replace in 6+ locations
- Verify tables/figures
- Update parameter descriptions

**Fix #3: Add Implementation Details (30-45 min)**
- Add new subsection
- Copy provided text
- Format equations

**TOTAL:** 1.5-2.5 hours

---

## ✅ VERIFICATION AFTER FIXES

**Run these checks:**

- [ ] CTRL+F for "skewed-$t$" in YOUR results → Should be 0 (literature mentions OK)
- [ ] CTRL+F for "sstd" → Should be 0 (or only in literature context)
- [ ] CTRL+F for "variance, $\sigma_t^2$" in TGARCH section → Should show std dev
- [ ] Equation 2.5 (TGARCH) uses $\sigma_t$ not $\sigma_t^2$
- [ ] All tables showing model names use "std" not "sstd"
- [ ] Methods section says "Normal and Student-$t$" not "skewed Student-$t$"

---

## 🎯 ALIGNMENT AFTER FIXES

**After implementing all fixes:**

✅ **TGARCH equation:** Matches code implementation exactly  
✅ **Distribution claims:** Accurately state norm and std only  
✅ **Skewness discussion:** Correctly attributed to NF, not parametric models  
✅ **All equations:** Match implementation  
✅ **All claims:** Supported by code

**No more discrepancies between dissertation and implementation!**

---

**Priority Level:** 🔴 HIGH - Must fix before submission  
**Risk if not fixed:** Reviewers will spot equation mismatch immediately  
**Difficulty:** Low - mostly text edits, no code changes needed

---
