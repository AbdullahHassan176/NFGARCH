# NF-GARCH Remediation Roadmap

**Generated**: 2025-01-XX

---

## Week 1: Critical Blockers

### Day 1-2: Fix Standardization Logic
- [ ] Create single standardization function
- [ ] Remove all redundant standardization
- [ ] Add unit tests for standardization
- [ ] Verify at each step (GARCH → NF → NF-GARCH)

### Day 3: Fix eGARCH E|z|
- [ ] Use theoretical E|z| for Normal distribution
- [ ] Use `E_abs_t(nu)` for Student-t distribution
- [ ] Update all eGARCH simulation code
- [ ] Add verification tests

### Day 4: Fix Platform Dependence
- [ ] Use environment variables for R/Python paths
- [ ] Create Linux/Mac shell scripts OR document Windows-only limitation
- [ ] Test on multiple platforms

### Day 5: Complete Dependency Lists
- [ ] Generate complete `sessionInfo()` output
- [ ] Pin exact package versions
- [ ] Synchronize all dependency lists

---

## Week 2: Major Issues

### Day 1-2: Fix Seed Management
- [ ] Add seeds to all scripts
- [ ] Centralize seed in `config.R`
- [ ] Document which operations are deterministic

### Day 3: Document Data Requirements
- [ ] Document data source
- [ ] Provide download instructions OR sample data
- [ ] Document data format and schema

### Day 4: Fix Installation Instructions
- [ ] Update README (remove non-existent files)
- [ ] Create missing files OR update references
- [ ] Test installation on clean machine

### Day 5: Verify AIC Claims
- [ ] Check actual AIC values in results
- [ ] Verify calculation method
- [ ] Clarify aggregation method
- [ ] Correct README claim (remove "4,500x")

---

## Week 3: Minor Issues & Documentation

### Day 1-2: Improve Error Handling
- [ ] Use consistent error handling
- [ ] Fail fast for critical errors
- [ ] Add input validation

### Day 3: Reduce Code Duplication
- [ ] Centralize common functions
- [ ] Remove deprecated code
- [ ] Improve code organization

### Day 4-5: Complete Documentation
- [ ] Add docstrings to all functions
- [ ] Document all assumptions
- [ ] Create reproducibility guide
- [ ] Add troubleshooting section

---

## Pre-MDPI Submission Checklist

- [ ] All 🔴 CRITICAL issues fixed
- [ ] All ⚠️ MAJOR issues fixed
- [ ] All tests passing
- [ ] Full reproducibility verified
- [ ] Documentation complete
- [ ] All claims verified
- [ ] Code reviewed by second person

---

**Estimated Time**: 3 weeks full-time work

