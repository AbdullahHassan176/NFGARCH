# Reviewer comments – addressed and manual checks

Summary of changes made in the dissertation and what to verify manually.

## Addressed in the thesis (done)

### 1. Flow flexibility / unconditional innovation (Issue 1)
- **Added** (Ch 1, after “highly flexible density representations…”):  
  *“In this dissertation, the learned innovation distribution is unconditional and state-invariant, and therefore does not model regime-dependent distributional dynamics.”*

### 2. Joint estimation (Issue 2)
- **Added** (Two-stage vs end-to-end):  
  *“Joint estimation was not pursued due to numerical instability and the need to isolate the marginal contribution of innovation modelling.”*

### 3. EGARCH notation (Issue 3)
- **Renamed** the coefficient in \(g(z_t)\) from \(b\) to \(\gamma\) (to avoid clash with GARCH \(b_j\)).
- **Updated** caption to: “The coefficient \(\gamma\) of the second term in (2.6) is set to 1 (\(\gamma = 1\)) in our formulation, avoiding notational conflict with the GARCH coefficients \(b_j\) in the variance recursion.”

### 4. TGARCH scale specification (Issue 4)
- **Added** (after scale vs variance recursion):  
  *“The scale specification was chosen to align with the rugarch implementation used in empirical estimation.”*

### 5. Identifiability (maths)
- **Added** formal statement of scale non-identifiability:  
  \(r_t = \sigma_t f_\theta(u_t)\), and if \(f_\theta\) includes a scaling component then \(\sigma_t f_\theta(u_t) = (\sigma_t c) \tilde{f}_\theta(u_t)\), implying partial scale non-identifiability.

### 6. Normalising flow density (Eq 2.8)
- **Added** the formulation in terms of the base variable and forward Jacobians:  
  \(p_{\mathbf{Z}_K}(\mathbf{z}_K) = p_\mathbf{X}(\mathbf{x}_0) \prod_{k=1}^{K} |\det J_{f_k}(\mathbf{z}_{k-1})|^{-1}\), with the existing inverse form kept as the displayed equation.

### 7. Look-ahead bias (Ch 3)
- **Added** (data split / flow training):  
  *“The flow is trained exclusively on residuals from the training set to prevent look-ahead bias.”*

### 8. VaR section
- **Added** at the start of the VaR section:  
  *“These are descriptive exceedance diagnostics rather than full model-based VaR backtests.”*

---

## Manual checks (you / Overleaf)

### Encoding / hyphen
- In Overleaf (or your editor), search for the character that appears as a small box or “￾” (e.g. Unicode U+FFFE or a special hyphen).
- Replace any such character with a normal hyphen `-`.
- Pay attention to words like “semi‑parametric”, “non‑parametric”, “flow‑after‑scaling”.

### Figure labels
- Figure labels in the template are already distinct: `fig:overview_flow`, `fig:overview_flow2`, etc. Confirm in the PDF that Figure 1.1 and 2.1 (or equivalent) have different `\label{}` and that no label is duplicated.

### Bibliography and citations
- You use **natbib** with `\textcite` and `\parencite`. Ensure you are not loading **biblatex**.
- Make citation style consistent (e.g. “(Author, year)” vs “Author (year)” as required by your style).

### Equation references
- Search for `\eqref{` and ensure every `\eqref{eq:...}` has a matching `\label{eq:...}` (e.g. for (2.1), (2.2), etc.).
- Compile and confirm no “??” or broken refs in the PDF.

---

## Reference

Source: structured academic + technical pass (conceptual correctness, internal consistency, mathematical clarity, methodology, LaTeX/Overleaf risks, examiner red flags).
