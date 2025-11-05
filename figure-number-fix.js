// ========================================
// UNIVERSAL FIGURE NUMBER FIX SCRIPT
// ========================================
// This script can be included in any chapter that needs figure number fixes

// GLOBAL BOOK CONFIGURATION - Update this for your entire book
const GLOBAL_FIGURE_MAPPING = {
    // Format: "original-chapter-position": "display-chapter-number"
    "8": "2",  // Figures from 8th position → display as Chapter 2  
    "10": "3",  // Figures from 10th position → display as Chapter 3
    // Add more mappings for your book here
};

// Each chapter should define: window.CHAPTER_CONFIG = { TARGET_CHAPTER: 1 }

// ========================================
// MAIN FUNCTIONS (DON'T MODIFY BELOW)
// ========================================

function fixAllFigureNumbers() {
    const TARGET_CHAPTER = window.CHAPTER_CONFIG?.TARGET_CHAPTER;
    
    // Fix figure captions
    const figcaptions = document.querySelectorAll('figcaption');
    figcaptions.forEach(function(caption) {
        let text = caption.innerHTML;
        
        // Apply global figure mapping
        Object.keys(GLOBAL_FIGURE_MAPPING).forEach(originalPos => {
            const displayChapter = GLOBAL_FIGURE_MAPPING[originalPos];
            text = text.replace(
                new RegExp(`Figure&nbsp;${originalPos}\\.(\\d+)`, 'g'),
                `Figure&nbsp;${displayChapter}.$1`
            );
            text = text.replace(
                new RegExp(`Figure\\s+${originalPos}\\.(\\d+)`, 'g'),
                `Figure ${displayChapter}.$1`
            );
        });
        
        caption.innerHTML = text;
    });
    
    // Fix cross-references
    const figureLinks = document.querySelectorAll('a[href*="#fig-"]');
    figureLinks.forEach(function(link) {
        const spans = link.querySelectorAll('span');
        
        spans.forEach(function(span) {
            let text = span.textContent;
            
            // Apply global figure mapping
            Object.keys(GLOBAL_FIGURE_MAPPING).forEach(originalPos => {
                const displayChapter = GLOBAL_FIGURE_MAPPING[originalPos];
                const regex = new RegExp(`^${originalPos}\\.(\\d+)$`);
                text = text.replace(regex, `${displayChapter}.$1`);
            });
            
            span.textContent = text;
        });
    });
}

function fixTooltipContent() {
    // Fix tooltip hover content
    if (window.tippy) {
        const figureLinks = document.querySelectorAll('a.quarto-xref[href*="#fig-"]');
        
        figureLinks.forEach(function(link) {
            const tippyInstance = link._tippy;
            
            if (tippyInstance) {
                const originalContentFn = tippyInstance.props.content;
                
                tippyInstance.setProps({
                    content: function() {
                        let content = originalContentFn ? originalContentFn.call(this) : '';
                        
                        // Apply global figure mapping
                        Object.keys(GLOBAL_FIGURE_MAPPING).forEach(originalPos => {
                            const displayChapter = GLOBAL_FIGURE_MAPPING[originalPos];
                            
                            content = content.replace(
                                new RegExp(`Figure&nbsp;${originalPos}\\.(\\d+)`, 'g'),
                                `Figure&nbsp;${displayChapter}.$1`
                            );
                            content = content.replace(
                                new RegExp(`Figure\\s+${originalPos}\\.(\\d+)`, 'g'),
                                `Figure ${displayChapter}.$1`
                            );
                        });
                        
                        return content;
                    }
                });
            }
        });
    }
}

function setupTooltipObserver() {
    // Watch for dynamically added tooltips
    const observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
            mutation.addedNodes.forEach(function(node) {
                if (node.nodeType === 1 && (node.classList?.contains('tippy-content') || node.querySelector?.('.tippy-content'))) {
                    const tooltipContent = node.classList?.contains('tippy-content') ? node : node.querySelector('.tippy-content');
                    
                    if (tooltipContent) {
                        let html = tooltipContent.innerHTML;
                        
                        // Apply global figure mapping
                        Object.keys(GLOBAL_FIGURE_MAPPING).forEach(originalPos => {
                            const displayChapter = GLOBAL_FIGURE_MAPPING[originalPos];
                            
                            html = html.replace(
                                new RegExp(`Figure&nbsp;${originalPos}\\.(\\d+)`, 'g'),
                                `Figure&nbsp;${displayChapter}.$1`
                            );
                            html = html.replace(
                                new RegExp(`Figure\\s+${originalPos}\\.(\\d+)`, 'g'),
                                `Figure ${displayChapter}.$1`
                            );
                        });
                        
                        tooltipContent.innerHTML = html;
                    }
                }
            });
        });
    });
    
    observer.observe(document.body, { childList: true, subtree: true });
}

function runAllFixes() {
    fixAllFigureNumbers();
    fixTooltipContent();
}

// ========================================
// EXECUTION (DON'T MODIFY)
// ========================================

// Run after DOM is loaded
document.addEventListener('DOMContentLoaded', function() {
    runAllFixes();
    setupTooltipObserver();
});

// Run with delays to catch Quarto scripts
setTimeout(runAllFixes, 100);
setTimeout(runAllFixes, 200);

// Run after window fully loads
window.addEventListener('load', function() {
    setTimeout(runAllFixes, 50);
});