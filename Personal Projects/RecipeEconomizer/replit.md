# Recipe Cost Optimizer

## Overview

Recipe Cost Optimizer is a Python web application that helps users find cheaper ingredient alternatives while maintaining nutritional value. The application integrates with the OpenNutrition MCP (Model Context Protocol) database containing 300,000+ food items to provide nutritional analysis and cost-based recommendations. Users can search ingredients, build recipes, analyze costs, and receive intelligent suggestions for budget-friendly substitutions with similar nutritional profiles.

## User Preferences

Preferred communication style: Simple, everyday language.

## System Architecture

### Frontend Architecture
- **Framework**: Vanilla JavaScript with server-side rendered HTML templates
- **Template Engine**: Jinja2 (Flask's built-in templating)
- **UI Design**: Single-page application pattern with dynamic content updates
- **Styling**: Custom CSS with gradient backgrounds and card-based layouts
- **Rationale**: Lightweight approach chosen for simplicity and minimal dependencies, avoiding frontend framework complexity while maintaining responsive user experience

### Backend Architecture
- **Framework**: Flask (Python web framework)
- **Application Structure**: Modular design with separated concerns
  - `app.py`: Main application routes and request handling
  - `database.py`: Data persistence layer with SQLite operations
  - `mcp_client.py`: External MCP server communication client
  - `recommendation_engine.py`: Business logic for ingredient alternatives
- **Communication Pattern**: RESTful JSON API for frontend-backend interaction
- **Session Management**: Flask session with configurable secret key from environment
- **Rationale**: Flask provides lightweight, flexible framework suitable for small to medium applications without excessive overhead

### Data Storage
- **Database**: SQLite (file-based relational database)
- **Schema Design**:
  - `ingredients` table: Stores food items with nutritional data (JSON), pricing, and categorization
  - `recipes` table: Stores user-created recipes with ingredient lists and total costs
- **Data Format**: Nutritional data stored as JSON text for flexibility with varying nutritional profiles
- **Rationale**: SQLite chosen for simplicity, zero-configuration setup, and sufficient performance for single-user or small-scale deployments. JSON storage allows flexible nutritional data structures without rigid schema constraints

### MCP Integration Architecture
- **Protocol**: OpenNutrition MCP server over stdio with JSON-RPC 2.0
- **Communication Method**: Line-based JSON communication via subprocess stdin/stdout
- **Message Format**: Raw JSON objects separated by newlines (no Content-Length framing)
- **Connection Model**: Request-response with message ID matching to filter notifications
- **Implementation**: Text-based I/O that skips non-JSON startup messages and parses JSON lines
- **Rationale**: MCP protocol enables integration with external nutrition database (326,759+ foods) while maintaining loose coupling. Simplified JSON line protocol for reliable cross-platform communication

### Recommendation Engine
- **Algorithm**: Nutritional profile similarity matching using multi-nutrient comparison (protein, carbs, fat, fiber, calories)
- **Pricing Strategy**: Deterministic pricing using stored values with keyword-based mock pricing fallback
- **Data Sources**: 
  - Primary: OpenNutrition MCP database for nutritional data
  - Secondary: Local SQLite cache for performance optimization
- **Rationale**: Two-tier approach balances real-time accuracy with performance, while mock pricing allows development/testing without external pricing APIs

## External Dependencies

### Third-Party Services
- **OpenNutrition MCP Server**: 
  - Technology: Node.js application
  - Purpose: Provides access to 300,000+ food items with detailed nutritional information
  - Integration: Local installation required, communicates via stdio protocol
  - Repository: https://github.com/deadletterq/mcp-opennutrition
  - Configuration: Path configured via `MCP_SERVER_PATH` environment variable

### Runtime Requirements
- **Python 3.11+**: Application runtime environment
- **Node.js v20+**: Required for OpenNutrition MCP server execution
- **npm**: Package manager for MCP server dependency installation

### Python Dependencies
- **Flask**: Web framework for routing and request handling
- **Requests**: HTTP library for future external API integrations
- **sqlite3**: Built-in Python module for database operations (no external dependency)
- **subprocess**: Built-in module for MCP server process management
- **json**: Built-in module for data serialization

### Recent Updates (October 17, 2025)
- **Node.js Installation**: Installed Node.js v20.19.3 for MCP server execution
- **MCP Server Setup**: Cloned and built OpenNutrition MCP server with 326,759 food items
- **Protocol Fix**: Updated MCP client to use line-based JSON protocol (not Content-Length framing) to match OpenNutrition server implementation
- **Price Consistency**: Updated recommendation engine to use stored prices instead of recalculating, ensuring accurate cost savings
- **Message ID Matching**: Added request/response correlation to filter out MCP server notifications
- **Defensive Fallbacks**: Added None-safe price handling for robust alternative recommendations
- **Windows Compatibility**: Added cross-platform Node.js detection using `shutil.which()`, Windows `node.exe` handling, and `CREATE_NO_WINDOW` flag to prevent console popup on Windows
- **MCP Path Configuration**: Set default MCP_SERVER_PATH to `/home/runner/workspace/mcp-opennutrition/build/index.js`

### Database
- **SQLite**: Embedded database, no separate server required
- **Location**: `ingredients.db` file in application directory
- **Tables**: ingredients (food catalog cache), recipes (user recipe storage)

### Configuration Dependencies
- **Environment Variables**:
  - `MCP_SERVER_PATH`: Absolute path to MCP server build file (index.js)
  - `SESSION_SECRET`: Flask session encryption key (defaults to 'dev-secret-key')