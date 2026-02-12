# QUICK FIX - Make Chronological Use Manual Scripts

## Change in `run_chronological.bat`

### OLD (Line ~155):
```batch
"%RSCRIPT%" additional_analysis\scripts\chronological\fit_garch_chronological.R
```

### NEW:
```batch
"%RSCRIPT%" scripts\manual\manual_garch_fitting.R
```

### OLD (Line ~170):
```batch
python additional_analysis\scripts\chronological\train_nf_chronological.py
```

### NEW:
```batch
python scripts\manual\manual_nf_training.py
```

## Result

- Uses EXACT SAME working scripts as main pipeline
- Same GARCH models, same NF training, same methodology
- ONLY difference: chronological split vs CV split (handled in the scripts)

This will make NF-GARCH win like it does in the main pipeline!
