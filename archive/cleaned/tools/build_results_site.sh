#!/bin/bash
# Build script for results site
# Generates manifest and creates index.html if it doesn't exist

set -e

echo "Building results site..."

# Run the Python script to generate manifest
python tools/generate_results_site.py

# Check if index.html exists, if not create a basic one
if [ ! -f "docs/index.html" ]; then
    echo "Creating basic index.html..."
    cat > docs/index.html << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>NF-GARCH Results Viewer</title>
    <style>
        body { font-family: system-ui, -apple-system, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }
        .container { max-width: 1200px; margin: 0 auto; background: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); overflow: hidden; }
        .header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; text-align: center; }
        .header h1 { margin: 0; font-size: 2.5em; font-weight: 300; }
        .content { padding: 30px; }
        .tabs { display: flex; background: #f8f9fa; border-bottom: 1px solid #dee2e6; }
        .tab { flex: 1; padding: 15px 20px; text-align: center; cursor: pointer; border: none; background: none; font-size: 1em; transition: all 0.3s ease; }
        .tab:hover { background: #e9ecef; }
        .tab.active { background: white; border-bottom: 3px solid #667eea; font-weight: 600; }
        .tab-content { display: none; }
        .tab-content.active { display: block; }
        .file-list { display: grid; gap: 10px; margin-top: 20px; }
        .file-item { background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #667eea; }
        .file-item a { color: #667eea; text-decoration: none; font-weight: 600; }
        .file-item a:hover { text-decoration: underline; }
        .file-meta { color: #6c757d; font-size: 0.9em; margin-top: 5px; }
        .plot-gallery { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 20px; margin-top: 20px; }
        .plot-item { background: white; border-radius: 8px; overflow: hidden; box-shadow: 0 2px 4px rgba(0,0,0,0.1); cursor: pointer; transition: transform 0.2s ease; }
        .plot-item:hover { transform: translateY(-2px); }
        .plot-item img { width: 100%; height: 200px; object-fit: cover; }
        .plot-info { padding: 15px; }
        .plot-title { font-weight: 600; margin-bottom: 5px; }
        .plot-path { color: #6c757d; font-size: 0.9em; }
        .loading { text-align: center; padding: 50px; color: #6c757d; }
        .error { color: #dc3545; text-align: center; padding: 20px; }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>NF-GARCH Results Viewer</h1>
            <p>Static Results and Plots Browser</p>
        </div>
        
        <div class="tabs">
            <button class="tab active" onclick="showTab('results')">Results</button>
            <button class="tab" onclick="showTab('plots')">Plots</button>
        </div>
        
        <div class="content">
            <div id="results" class="tab-content active">
                <h2>Result Files</h2>
                <div id="results-list" class="loading">Loading results...</div>
            </div>
            
            <div id="plots" class="tab-content">
                <h2>Plots Gallery</h2>
                <div id="plots-gallery" class="loading">Loading plots...</div>
            </div>
        </div>
    </div>
    
    <script>
        let manifest = null;
        
        // Load manifest and initialize
        async function loadManifest() {
            try {
                const response = await fetch('manifest.json');
                manifest = await response.json();
                initializeViewer();
            } catch (error) {
                console.error('Error loading manifest:', error);
                document.getElementById('results-list').innerHTML = '<div class="error">Error loading manifest. Make sure manifest.json exists.</div>';
                document.getElementById('plots-gallery').innerHTML = '<div class="error">Error loading manifest. Make sure manifest.json exists.</div>';
            }
        }
        
        function initializeViewer() {
            displayResults();
            displayPlots();
        }
        
        function displayResults() {
            const resultsList = document.getElementById('results-list');
            const results = manifest.results || [];
            
            if (results.length === 0) {
                resultsList.innerHTML = '<div class="error">No result files found.</div>';
                return;
            }
            
            const html = results.map(file => `
                <div class="file-item">
                    <a href="${file.path}" target="_blank">${file.path}</a>
                    <div class="file-meta">
                        Size: ${formatFileSize(file.size)} | Modified: ${new Date(file.modified).toLocaleDateString()}
                    </div>
                </div>
            `).join('');
            
            resultsList.innerHTML = html;
        }
        
        function displayPlots() {
            const plotsGallery = document.getElementById('plots-gallery');
            const plots = manifest.plots || [];
            
            if (plots.length === 0) {
                plotsGallery.innerHTML = '<div class="error">No plot files found.</div>';
                return;
            }
            
            const html = plots.map(plot => `
                <div class="plot-item" onclick="openLightbox('${plot.path}')">
                    <img src="${plot.path}" alt="${plot.path}" loading="lazy">
                    <div class="plot-info">
                        <div class="plot-title">${Path.basename(plot.path)}</div>
                        <div class="plot-path">${plot.path}</div>
                    </div>
                </div>
            `).join('');
            
            plotsGallery.innerHTML = html;
        }
        
        function showTab(tabName) {
            // Hide all tab contents
            document.querySelectorAll('.tab-content').forEach(content => {
                content.classList.remove('active');
            });
            
            // Remove active class from all tabs
            document.querySelectorAll('.tab').forEach(tab => {
                tab.classList.remove('active');
            });
            
            // Show selected tab content
            document.getElementById(tabName).classList.add('active');
            
            // Add active class to clicked tab
            event.target.classList.add('active');
        }
        
        function openLightbox(imageSrc) {
            // Simple lightbox implementation
            const lightbox = document.createElement('div');
            lightbox.style.cssText = `
                position: fixed; top: 0; left: 0; width: 100%; height: 100%;
                background: rgba(0,0,0,0.8); z-index: 1000; display: flex;
                align-items: center; justify-content: center; cursor: pointer;
            `;
            lightbox.onclick = () => document.body.removeChild(lightbox);
            
            const img = document.createElement('img');
            img.src = imageSrc;
            img.style.cssText = 'max-width: 90%; max-height: 90%; object-fit: contain;';
            
            lightbox.appendChild(img);
            document.body.appendChild(lightbox);
        }
        
        function formatFileSize(bytes) {
            if (bytes === 0) return '0 Bytes';
            const k = 1024;
            const sizes = ['Bytes', 'KB', 'MB', 'GB'];
            const i = Math.floor(Math.log(bytes) / Math.log(k));
            return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
        }
        
        // Initialize when page loads
        document.addEventListener('DOMContentLoaded', loadManifest);
    </script>
</body>
</html>
EOF
    echo "Created basic index.html"
else
    echo "index.html already exists, skipping creation"
fi

echo "Results site build complete!"
