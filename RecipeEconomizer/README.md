# Recipe Cost Optimizer

A Python web application that analyzes recipes using the OpenNutrition MCP database and recommends cheaper alternative ingredients while maintaining similar nutritional profiles. Created using Replit.

## Features

- 🔍 **Ingredient Search**: Search through 300,000+ food items from the OpenNutrition database
- 📊 **Nutritional Analysis**: Fetch detailed nutritional data for each ingredient
- 💰 **Cost Analysis**: Calculate recipe costs and identify savings opportunities
- 🔄 **Smart Alternatives**: Find cheaper ingredients with similar nutritional profiles
- 📝 **Recipe Cataloging**: Store and track analyzed recipes in a local database
- 🎨 **Modern UI**: Clean, responsive web interface

## Prerequisites

1. **Python 3.11+** (already installed)
2. **Node.js v20+** (required for OpenNutrition MCP server)
   - Windows: Download from [nodejs.org](https://nodejs.org/)
   - macOS: Install via Homebrew (`brew install node`) or download from nodejs.org
   - Linux: Install via package manager or nvm
3. **OpenNutrition MCP Server** (local installation)

## Setup Instructions

### Step 1: Install OpenNutrition MCP Server

#### On Windows (PowerShell or Command Prompt):
```bash
# Clone the OpenNutrition MCP server repository
git clone https://github.com/deadletterq/mcp-opennutrition
cd mcp-opennutrition

# Install dependencies
npm install

# Build the server
npm run build
```

#### On macOS/Linux:
```bash
# Clone the OpenNutrition MCP server repository
git clone https://github.com/deadletterq/mcp-opennutrition
cd mcp-opennutrition

# Install dependencies
npm install

# Build the server
npm run build
```

### Step 2: Configure the MCP Server Path

The application automatically detects Node.js on your system (cross-platform). Set the MCP server path:

#### On Windows:
```bash
# PowerShell
$env:MCP_SERVER_PATH="C:\path\to\mcp-opennutrition\build\index.js"

# Command Prompt
set MCP_SERVER_PATH=C:\path\to\mcp-opennutrition\build\index.js
```

#### On macOS/Linux:
```bash
export MCP_SERVER_PATH="/absolute/path/to/mcp-opennutrition/build/index.js"
```

**Or** update it directly in `app.py` (line 12):

```python
# Windows example
MCP_SERVER_PATH = r'C:\Users\YourName\mcp-opennutrition\build\index.js'

# macOS/Linux example  
MCP_SERVER_PATH = '/home/yourname/mcp-opennutrition/build/index.js'
```

### Step 3: Run the Application

The Flask application is already configured to run automatically. Simply access it in your browser.

## How to Use

1. **Enter Recipe Name**: Give your recipe a name
2. **Search Ingredients**: Type ingredient names to search the OpenNutrition database
3. **Add Ingredients**: Click on search results and specify amounts (in grams)
4. **Analyze Recipe**: Click "Analyze Recipe" to get cost analysis and recommendations
5. **View Alternatives**: See cheaper ingredient alternatives with nutritional similarity scores

## Application Structure

```
.
├── app.py                      # Main Flask application
├── mcp_client.py              # OpenNutrition MCP client
├── database.py                # SQLite catalog database
├── recommendation_engine.py   # Alternative recommendation algorithm
├── templates/
│   └── index.html            # Web interface
├── static/
│   └── style.css            # Styling
└── ingredients.db           # SQLite database (auto-created)
```

## How It Works

1. **MCP Protocol**: The app connects to the OpenNutrition MCP server using JSON-RPC over stdio with Content-Length framed messages
2. **Search**: Sends search requests to find ingredients in the 300,000+ food database
3. **Fetch Data**: Retrieves detailed nutritional information for selected ingredients
4. **Catalog**: Stores ingredient data in a local SQLite database for future reference
5. **Analyze**: Calculates costs using mock pricing (customizable for real grocery APIs)
6. **Recommend**: Finds cheaper alternatives by comparing nutritional similarity scores (protein, carbs, fat, fiber, calories)
7. **Display**: Shows original ingredients vs. cheaper alternatives with cost savings and nutritional matches

## Mock Pricing

The application currently uses mock pricing data for demonstration. To use real prices:

1. Integrate a grocery API (e.g., Kroger, Instacart, Walmart)
2. Update `recommendation_engine.py` to fetch real-time prices
3. Replace the `get_mock_price()` method with actual API calls

## Database Schema

### Ingredients Table
- `id`: Auto-increment primary key
- `food_id`: Unique food ID from OpenNutrition
- `name`: Ingredient name
- `nutritional_data`: JSON nutritional information
- `price_per_100g`: Price per 100 grams
- `category`: Food category
- `created_at`: Timestamp

### Recipes Table
- `id`: Auto-increment primary key
- `name`: Recipe name
- `ingredients`: JSON list of ingredients
- `total_cost`: Total recipe cost
- `created_at`: Timestamp

## Cross-Platform Compatibility

This application works on **Windows, macOS, and Linux**:

- **Automatic Node.js detection**: Uses `shutil.which()` to find Node.js automatically
- **Windows compatibility**: Handles `node.exe` and prevents console window popup
- **Path handling**: Supports both Windows (`C:\...`) and Unix (`/...`) paths
- **Binary I/O**: UTF-8 safe communication for international ingredient names

## Troubleshooting

### "Failed to connect to MCP server"

**All platforms:**
- Ensure Node.js v20+ is installed: `node --version` or `node -v`
- Verify the MCP server is built: check if `mcp-opennutrition/build/index.js` exists
- Update `MCP_SERVER_PATH` environment variable or in `app.py`
- Check MCP server path is absolute, not relative

**Windows-specific:**
- Make sure Node.js is in your PATH (restart terminal after installation)
- Use full path with backslashes: `C:\Users\YourName\mcp-opennutrition\build\index.js`
- Use raw string in Python: `r'C:\path\to\file'` or escape backslashes: `'C:\\path\\to\\file'`

**macOS/Linux-specific:**
- Ensure Node.js is accessible from terminal: `which node`
- Use absolute paths starting with `/`: `/home/user/mcp-opennutrition/build/index.js`

### "No results found"

- Verify the OpenNutrition MCP server is properly installed
- Try searching for common foods: "chicken", "rice", "apple"
- Check that the MCP server database is downloaded

### Port 5000 already in use

- The application is configured to run on port 5000
- Stop other services using this port
- Or update `app.py` to use a different port

## Future Enhancements

- Real-time grocery pricing integration
- User accounts for saved recipes
- Advanced nutritional matching
- Dietary restriction filters
- Batch recipe analysis for meal planning
- Barcode scanning support
- Export recipes to PDF/CSV

## License

MIT
