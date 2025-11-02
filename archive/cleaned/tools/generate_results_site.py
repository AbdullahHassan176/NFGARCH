#!/usr/bin/env python3
"""
Generate results site manifest for static HTML viewer.
Scans results/ and outputs/ directories and creates a JSON manifest.
"""

import os
import json
import sys
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any


def get_file_info(file_path: Path) -> Dict[str, Any]:
    """Get file information including size and type."""
    try:
        stat = file_path.stat()
        return {
            "path": str(file_path.relative_to(Path.cwd())),
            "size": stat.st_size,
            "modified": datetime.fromtimestamp(stat.st_mtime).isoformat()
        }
    except (OSError, ValueError):
        return None


def scan_directory(directory: Path, extensions: List[str]) -> List[Dict[str, Any]]:
    """Scan directory for files with specified extensions."""
    files = []
    
    if not directory.exists():
        return files
    
    for file_path in directory.rglob("*"):
        if file_path.is_file() and file_path.suffix.lower() in extensions:
            file_info = get_file_info(file_path)
            if file_info:
                files.append(file_info)
    
    return files


def generate_manifest() -> Dict[str, Any]:
    """Generate the manifest JSON."""
    base_dir = Path.cwd()
    
    # Define file types to scan
    result_extensions = ['.csv', '.json', '.txt', '.html', '.xlsx']
    plot_extensions = ['.png', '.jpg', '.jpeg', '.svg', '.gif']
    
    # Scan directories
    results_files = scan_directory(base_dir / "results", result_extensions)
    outputs_files = scan_directory(base_dir / "outputs", result_extensions)
    plot_files = scan_directory(base_dir / "outputs", plot_extensions)
    
    # Also scan results/plots if it exists
    results_plots = scan_directory(base_dir / "results" / "plots", plot_extensions)
    plot_files.extend(results_plots)
    
    manifest = {
        "generated_at": datetime.now().isoformat(),
        "results": results_files + outputs_files,
        "plots": plot_files,
        "summary": {
            "total_results": len(results_files + outputs_files),
            "total_plots": len(plot_files),
            "total_size": sum(f["size"] for f in results_files + outputs_files + plot_files)
        }
    }
    
    return manifest


def main():
    """Main entry point."""
    try:
        # Ensure docs directory exists
        docs_dir = Path("docs")
        docs_dir.mkdir(exist_ok=True)
        
        # Generate manifest
        manifest = generate_manifest()
        
        # Write manifest
        manifest_path = docs_dir / "manifest.json"
        with open(manifest_path, 'w', encoding='utf-8') as f:
            json.dump(manifest, f, indent=2)
        
        print(f"Generated manifest with {manifest['summary']['total_results']} result files and {manifest['summary']['total_plots']} plot files")
        print(f"Manifest saved to: {manifest_path}")
        
        return 0
        
    except Exception as e:
        print(f"Error generating manifest: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
