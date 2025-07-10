// MCPFM Website Interactive Functionality

document.addEventListener('DOMContentLoaded', function() {
    // Initialize all interactive components
    initializeNavigation();
    initializeTabSwitching();
    initializeScrollEffects();
    initializeAnimations();
    initializeTheme();
});

// Navigation functionality
function initializeNavigation() {
    const navToggle = document.getElementById('nav-toggle');
    const navMenu = document.getElementById('nav-menu');
    const navLinks = document.querySelectorAll('.nav-link');

    // Mobile menu toggle
    if (navToggle) {
        navToggle.addEventListener('click', function() {
            navMenu.classList.toggle('active');
            navToggle.classList.toggle('active');
        });
    }

    // Close menu when clicking on links
    navLinks.forEach(link => {
        link.addEventListener('click', function() {
            navMenu.classList.remove('active');
            navToggle.classList.remove('active');
        });
    });

    // Smooth scrolling for anchor links
    navLinks.forEach(link => {
        if (link.getAttribute('href').startsWith('#')) {
            link.addEventListener('click', function(e) {
                e.preventDefault();
                const targetId = this.getAttribute('href').substring(1);
                const targetElement = document.getElementById(targetId);
                
                if (targetElement) {
                    const headerHeight = document.querySelector('.navbar').offsetHeight;
                    const targetPosition = targetElement.offsetTop - headerHeight - 20;
                    
                    window.scrollTo({
                        top: targetPosition,
                        behavior: 'smooth'
                    });
                }
            });
        }
    });

    // Active navigation highlighting
    window.addEventListener('scroll', updateActiveNavigation);
}

// Update active navigation based on scroll position
function updateActiveNavigation() {
    const sections = document.querySelectorAll('section[id]');
    const navLinks = document.querySelectorAll('.nav-link[href^="#"]');
    const headerHeight = document.querySelector('.navbar').offsetHeight;
    
    let activeSection = '';
    
    sections.forEach(section => {
        const sectionTop = section.offsetTop - headerHeight - 100;
        const sectionBottom = sectionTop + section.offsetHeight;
        
        if (window.scrollY >= sectionTop && window.scrollY < sectionBottom) {
            activeSection = section.getAttribute('id');
        }
    });
    
    navLinks.forEach(link => {
        link.classList.remove('active');
        if (link.getAttribute('href') === `#${activeSection}`) {
            link.classList.add('active');
        }
    });
}

// Tab switching functionality for examples section
function initializeTabSwitching() {
    const tabButtons = document.querySelectorAll('.tab-button');
    const tabPanels = document.querySelectorAll('.example-panel');

    tabButtons.forEach(button => {
        button.addEventListener('click', function() {
            const targetTab = this.getAttribute('data-tab');
            
            // Remove active class from all buttons and panels
            tabButtons.forEach(btn => btn.classList.remove('active'));
            tabPanels.forEach(panel => panel.classList.remove('active'));
            
            // Add active class to clicked button and corresponding panel
            this.classList.add('active');
            const targetPanel = document.getElementById(targetTab);
            if (targetPanel) {
                targetPanel.classList.add('active');
            }
        });
    });
}

// Scroll effects and animations
function initializeScrollEffects() {
    // Navbar scroll effect
    const navbar = document.querySelector('.navbar');
    let lastScrollY = window.scrollY;

    window.addEventListener('scroll', function() {
        const currentScrollY = window.scrollY;
        
        if (currentScrollY > 100) {
            navbar.style.background = 'rgba(255, 255, 255, 0.98)';
            navbar.style.backdropFilter = 'blur(15px)';
            navbar.style.boxShadow = '0 4px 6px -1px rgb(0 0 0 / 0.1)';
        } else {
            navbar.style.background = 'rgba(255, 255, 255, 0.95)';
            navbar.style.backdropFilter = 'blur(10px)';
            navbar.style.boxShadow = 'none';
        }
        
        lastScrollY = currentScrollY;
    });

    // Intersection Observer for fade-in animations
    const observerOptions = {
        threshold: 0.1,
        rootMargin: '0px 0px -50px 0px'
    };

    const observer = new IntersectionObserver(function(entries) {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.classList.add('animate-in');
            }
        });
    }, observerOptions);

    // Observe elements for animation
    const animateElements = document.querySelectorAll('.overview-card, .feature-item, .doc-card, .step');
    animateElements.forEach(el => {
        el.style.opacity = '0';
        el.style.transform = 'translateY(30px)';
        el.style.transition = 'opacity 0.6s ease, transform 0.6s ease';
        observer.observe(el);
    });
}

// Initialize animations
function initializeAnimations() {
    // Add CSS for animate-in class
    const style = document.createElement('style');
    style.textContent = `
        .animate-in {
            opacity: 1 !important;
            transform: translateY(0) !important;
        }
        
        .animate-in:nth-child(1) { transition-delay: 0.1s; }
        .animate-in:nth-child(2) { transition-delay: 0.2s; }
        .animate-in:nth-child(3) { transition-delay: 0.3s; }
        .animate-in:nth-child(4) { transition-delay: 0.4s; }
    `;
    document.head.appendChild(style);

    // Counter animation for hero stats
    animateCounters();
    
    // Network visualization animation
    animateNetworkNodes();
}

// Animate counter numbers in hero section
function animateCounters() {
    const counters = document.querySelectorAll('.stat-number');
    
    const animateCounter = (counter) => {
        const target = counter.textContent;
        const numericTarget = parseInt(target.replace(/\D/g, ''));
        const suffix = target.replace(/[\d,]/g, '');
        let current = 0;
        const increment = numericTarget / 50;
        const timer = setInterval(() => {
            current += increment;
            if (current >= numericTarget) {
                counter.textContent = target;
                clearInterval(timer);
            } else {
                counter.textContent = Math.floor(current).toLocaleString() + suffix;
            }
        }, 30);
    };

    // Trigger counter animation when hero section is visible
    const heroObserver = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                counters.forEach(counter => animateCounter(counter));
                heroObserver.unobserve(entry.target);
            }
        });
    });

    const heroSection = document.querySelector('.hero');
    if (heroSection) {
        heroObserver.observe(heroSection);
    }
}

// Animate network nodes in hero section
function animateNetworkNodes() {
    const nodes = document.querySelectorAll('.node');
    const connections = document.querySelectorAll('.connection');
    
    // Add staggered animation delays
    nodes.forEach((node, index) => {
        node.style.animationDelay = `${index * 0.4}s`;
    });
    
    connections.forEach((connection, index) => {
        connection.style.animationDelay = `${index * 0.6}s`;
    });
}

// Theme and visual enhancements
function initializeTheme() {
    // Add hover effects to cards
    const cards = document.querySelectorAll('.overview-card, .doc-card, .feature-visual');
    
    cards.forEach(card => {
        card.addEventListener('mouseenter', function() {
            this.style.transform = 'translateY(-8px) scale(1.02)';
        });
        
        card.addEventListener('mouseleave', function() {
            this.style.transform = 'translateY(0) scale(1)';
        });
    });

    // Add ripple effect to buttons
    const buttons = document.querySelectorAll('.btn, .tab-button');
    
    buttons.forEach(button => {
        button.addEventListener('click', function(e) {
            const ripple = document.createElement('span');
            const rect = this.getBoundingClientRect();
            const size = Math.max(rect.width, rect.height);
            const x = e.clientX - rect.left - size / 2;
            const y = e.clientY - rect.top - size / 2;
            
            ripple.style.cssText = `
                position: absolute;
                border-radius: 50%;
                background: rgba(255, 255, 255, 0.3);
                width: ${size}px;
                height: ${size}px;
                left: ${x}px;
                top: ${y}px;
                animation: ripple 0.6s ease-out;
                pointer-events: none;
            `;
            
            this.style.position = 'relative';
            this.style.overflow = 'hidden';
            this.appendChild(ripple);
            
            setTimeout(() => {
                ripple.remove();
            }, 600);
        });
    });

    // Add CSS for ripple animation
    const rippleStyle = document.createElement('style');
    rippleStyle.textContent = `
        @keyframes ripple {
            from {
                opacity: 1;
                transform: scale(0);
            }
            to {
                opacity: 0;
                transform: scale(2);
            }
        }
    `;
    document.head.appendChild(rippleStyle);
}

// Utility functions
function debounce(func, wait) {
    let timeout;
    return function executedFunction(...args) {
        const later = () => {
            clearTimeout(timeout);
            func(...args);
        };
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
    };
}

// Smooth scroll polyfill for older browsers
function smoothScrollTo(target, duration = 1000) {
    const targetElement = document.querySelector(target);
    if (!targetElement) return;
    
    const targetPosition = targetElement.offsetTop - document.querySelector('.navbar').offsetHeight;
    const startPosition = window.pageYOffset;
    const distance = targetPosition - startPosition;
    let startTime = null;
    
    function animation(currentTime) {
        if (startTime === null) startTime = currentTime;
        const timeElapsed = currentTime - startTime;
        const run = ease(timeElapsed, startPosition, distance, duration);
        window.scrollTo(0, run);
        if (timeElapsed < duration) requestAnimationFrame(animation);
    }
    
    function ease(t, b, c, d) {
        t /= d / 2;
        if (t < 1) return c / 2 * t * t + b;
        t--;
        return -c / 2 * (t * (t - 2) - 1) + b;
    }
    
    requestAnimationFrame(animation);
}

// Copy code functionality
function initializeCodeCopy() {
    const codeBlocks = document.querySelectorAll('pre code');
    
    codeBlocks.forEach(codeBlock => {
        const copyButton = document.createElement('button');
        copyButton.textContent = 'Copy';
        copyButton.className = 'copy-button';
        copyButton.style.cssText = `
            position: absolute;
            top: 10px;
            right: 10px;
            background: rgba(255, 255, 255, 0.1);
            border: 1px solid rgba(255, 255, 255, 0.2);
            color: white;
            padding: 5px 10px;
            border-radius: 4px;
            cursor: pointer;
            font-size: 12px;
            transition: all 0.2s ease;
        `;
        
        const container = codeBlock.closest('pre');
        container.style.position = 'relative';
        container.appendChild(copyButton);
        
        copyButton.addEventListener('click', async function() {
            try {
                await navigator.clipboard.writeText(codeBlock.textContent);
                this.textContent = 'Copied!';
                this.style.background = 'rgba(16, 185, 129, 0.2)';
                this.style.borderColor = 'rgba(16, 185, 129, 0.3)';
                
                setTimeout(() => {
                    this.textContent = 'Copy';
                    this.style.background = 'rgba(255, 255, 255, 0.1)';
                    this.style.borderColor = 'rgba(255, 255, 255, 0.2)';
                }, 2000);
            } catch (err) {
                console.error('Failed to copy code: ', err);
            }
        });
        
        copyButton.addEventListener('mouseenter', function() {
            this.style.background = 'rgba(255, 255, 255, 0.2)';
        });
        
        copyButton.addEventListener('mouseleave', function() {
            if (this.textContent === 'Copy') {
                this.style.background = 'rgba(255, 255, 255, 0.1)';
            }
        });
    });
}

// Initialize code copying after a short delay to ensure DOM is ready
setTimeout(initializeCodeCopy, 500);

// Performance optimization
const debouncedScroll = debounce(() => {
    updateActiveNavigation();
}, 10);

window.addEventListener('scroll', debouncedScroll, { passive: true });

// Print debug info
console.log('🚀 MCPFM Website loaded successfully!');
console.log('📊 Interactive features initialized');
console.log('🎨 Animations and scroll effects active');

// Export functions for potential external use
window.MCPFMWebsite = {
    smoothScrollTo,
    updateActiveNavigation,
    animateCounters
};