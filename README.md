# MCPFM: Model Context Protocol Financial Markets

[![R package](https://img.shields.io/badge/R-package-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Status: Production Ready](https://img.shields.io/badge/Status-Production%20Ready-green.svg)](https://github.com/avishekb9/MCPFM)

## Overview

MCPFM is an advanced R package for analyzing multi-scale network dynamics and systemic risk in financial markets through a novel Model Context Protocol (MCP) approach. This package provides comprehensive tools for financial network analysis, agent-based modeling, and macroprudential policy simulation.

## Key Features

### 🚀 **Core Capabilities**
- **MCP Agent Communication Protocol** - Novel framework for heterogeneous agent interactions
- **Multi-Scale Network Dynamics** - Wavelet-based analysis across temporal scales
- **Transfer Entropy Networks** - Information flow measurement with parallel processing
- **Systemic Risk Assessment** - Comprehensive 5-component risk index
- **Agent-Based Market Simulation** - Realistic multi-agent financial market modeling
- **Policy Simulation Framework** - Macroprudential analysis and optimization tools

### 📊 **Advanced Analytics**
- Real-time systemic risk monitoring with configurable thresholds
- Transfer entropy calculation with statistical significance testing
- Multi-scale network evolution analysis using wavelet decomposition
- Heterogeneous agent modeling (HFT, Market Makers, Institutional Investors, Regulators)
- Policy effectiveness evaluation with cost-benefit analysis

### 📈 **Professional Visualizations**
- Interactive network graphs and heatmaps
- Risk evolution time series with regime detection
- Transfer entropy network visualizations
- Market dynamics and agent performance plots
- Automated report generation (Markdown + JSON)

## Installation

### From GitHub (Recommended)
```r
# Install devtools if not already installed
install.packages("devtools")

# Install MCPFM package
devtools::install_github("avishekb9/MCPFM")
```

### Dependencies
The package requires R >= 4.0.0 and several dependencies including:
- Core: `igraph`, `data.table`, `parallel`, `ggplot2`
- Financial: `quantmod`, `PerformanceAnalytics`, `rugarch`
- Network: `sna`, `network`, `visNetwork`
- ML/Stats: `randomForest`, `xgboost`, `boot`

All dependencies are automatically installed.

## Quick Start

### Basic Usage
```r
library(MCPFM)

# Create MCP agents
hft_agent <- createMCPAgent("HFT_001", "HFT", capital_base = 1e6)
mm_agent <- createMCPAgent("MM_001", "MM", capital_base = 10e6)

# Establish connections
connection <- establishMCPConnection(hft_agent, mm_agent, strength = 0.7)

# Calculate transfer entropy network
te_result <- calculateTransferEntropy(your_market_data)

# Assess systemic risk
risk_assessment <- calculateSystemicRisk(agent_system, te_network = te_result)

# Visualize results
plotNetworkDynamics(network_dynamics, plot_type = "network")
visualizeRiskEvolution(risk_data, plot_type = "timeseries")
```

### Complete Example
```r
# Initialize agent-based system
agent_system <- initializeAgentSystem(
  n_hft = 20, n_mm = 8, n_ii = 12, n_reg = 2
)

# Run simulation
simulation_result <- runAgentSimulation(agent_system)

# Analyze systemic risk
systemic_risk <- calculateSystemicRisk(simulation_result)

# Generate comprehensive report
risk_report <- generateRiskReport(simulation_result)

# Policy simulation
policy_result <- simulatePolicy(
  agent_system, 
  policy_type = "communication_tax",
  policy_parameters = list(rate = 0.001)
)
```

## Package Structure

```
MCPFM/
├── R/
│   ├── mcp_protocol.R          # Agent communication framework
│   ├── network_dynamics.R      # Multi-scale network modeling
│   ├── transfer_entropy.R      # Information flow calculation
│   ├── agent_based_model.R     # Heterogeneous agent simulation
│   ├── systemic_risk.R         # Risk assessment and monitoring
│   ├── visualization.R         # Advanced plotting functions
│   └── policy_simulation.R     # Macroprudential analysis
├── data/                       # Example datasets
├── man/                        # Documentation
├── tests/                      # Test suite
├── vignettes/                  # Usage guides
└── test_outputs/              # Example outputs
    ├── data/                  # Test data files
    ├── figures/               # Generated visualizations
    └── reports/               # Analysis reports
```

## Test Results

The package has been comprehensively tested with realistic financial market data:

- ✅ **4,000+ lines** of production-ready R code
- ✅ **19 output files** generated (data, figures, reports)
- ✅ **Multi-agent simulation** with 4 agent types
- ✅ **Transfer entropy network** analysis
- ✅ **Systemic risk assessment** with monitoring
- ✅ **Professional visualizations** and reporting

### Key Test Metrics
- **Systemic Risk Index:** 0.280 (Moderate)
- **Network Density:** 31.2%
- **Information Flow Links:** 5 significant connections
- **Agent Types:** HFT, Market Makers, Institutional Investors, Regulators

## Academic Research

This package supports the research paper:
**"Multi-Scale Network Dynamics and Systemic Risk: A Model Context Protocol Approach to Financial Markets"**

### Research Contributions
1. **Novel MCP Framework** for financial agent communication
2. **Multi-Scale Network Analysis** using wavelet decomposition
3. **Integrated Systemic Risk Assessment** across multiple components
4. **Policy Simulation Tools** for macroprudential research

## Applications

### 🏛️ **Regulatory Analysis**
- Macroprudential policy design and evaluation
- Systemic risk monitoring and early warning systems
- Stress testing and scenario analysis
- Market structure impact assessment

### 🏦 **Financial Institutions**
- Portfolio risk management
- Network exposure analysis
- Agent-based market modeling
- Trading strategy evaluation

### 🎓 **Academic Research**
- Computational finance studies
- Network economics research
- Agent-based modeling applications
- Policy effectiveness analysis

## Documentation

### Function Reference
Complete documentation for all 50+ functions is available:
```r
# View package documentation
?MCPFM

# Function-specific help
?calculateSystemicRisk
?runAgentSimulation
?plotNetworkDynamics
```

### Vignettes
Comprehensive usage guides are included:
- **Getting Started** - Basic package usage
- **Network Analysis** - Advanced network modeling
- **Risk Assessment** - Systemic risk calculation
- **Policy Simulation** - Macroprudential analysis

## Performance

- **Execution Time:** Optimized for real-time analysis
- **Scalability:** Designed for 100+ agents and 50+ assets
- **Memory Efficiency:** Sparse matrix representations
- **Parallel Processing:** Multi-core transfer entropy calculation

## Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details.

### Development Setup
```bash
git clone https://github.com/avishekb9/MCPFM.git
cd MCPFM
R CMD build .
R CMD check .
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use this package in your research, please cite:

```bibtex
@misc{mcpfm2025,
  title={MCPFM: Model Context Protocol Financial Markets},
  author={Avishek Bhandari},
  year={2025},
  url={https://github.com/avishekb9/MCPFM},
  note={R package version 1.0.0}
}
```

## Support

- 📧 **Email:** research@waveqte.org
- 🐛 **Issues:** [GitHub Issues](https://github.com/avishekb9/MCPFM/issues)
- 📚 **Documentation:** [Package Website](https://avishekb9.github.io/MCPFM/)

## Related Projects

- [WaveQTEX](https://github.com/avishekb9/WaveQTEX) - Web interface for MCPFM
- [Financial Network Analysis](https://github.com/WaveQTE) - Related research tools

---

**MCPFM Package** | **Version 1.0.0** | **Production Ready**  
*Advanced Financial Network Analysis for the Modern Era*
