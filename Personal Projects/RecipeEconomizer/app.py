from flask import Flask, render_template, request, jsonify
import os
from database import IngredientCatalog
from recommendation_engine import RecommendationEngine

app = Flask(__name__)
app.secret_key = os.environ.get('SESSION_SECRET', 'dev-secret-key')

catalog = IngredientCatalog()
recommendation_engine = RecommendationEngine()

MCP_SERVER_PATH = os.environ.get('MCP_SERVER_PATH', 'C:\\Users\\liron\\Desktop\\RecipeEconomizer (2)\\RecipeEconomizer\\mcp-opennutrition\\build\\index.js')

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/search_ingredient', methods=['POST'])
def search_ingredient():
    data = request.json
    ingredient_name = data.get('name', '')
    
    if not ingredient_name:
        return jsonify({'error': 'Ingredient name is required'}), 400
    
    try:
        from mcp_client import OpenNutritionMCPClient
        
        mcp_client = OpenNutritionMCPClient(MCP_SERVER_PATH)
        if mcp_client.start_connection():
            foods = mcp_client.search_food_by_name(ingredient_name, limit=5)
            mcp_client.close_connection()
            
            if foods:
                results = []
                for food in foods:
                    food_id = food.get('id', '')
                    food_name = food.get('name', '')
                    
                    results.append({
                        'id': food_id,
                        'name': food_name,
                        'brand': food.get('brand', ''),
                        'category': food.get('category', 'Unknown')
                    })
                
                return jsonify({'results': results})
            else:
                return jsonify({'results': []})
        else:
            return jsonify({'error': 'Failed to connect to MCP server. Please ensure the OpenNutrition MCP server is properly configured.'}), 500
            
    except Exception as e:
        return jsonify({'error': f'Search failed: {str(e)}'}), 500

@app.route('/analyze_recipe', methods=['POST'])
def analyze_recipe():
    data = request.json
    recipe_name = data.get('recipe_name', 'My Recipe')
    ingredients = data.get('ingredients', [])
    
    if not ingredients:
        return jsonify({'error': 'No ingredients provided'}), 400
    
    try:
        from mcp_client import OpenNutritionMCPClient
        
        mcp_client = OpenNutritionMCPClient(MCP_SERVER_PATH)
        if not mcp_client.start_connection():
            return jsonify({'error': 'Failed to connect to MCP server'}), 500
        
        analyzed_ingredients = []
        all_foods_for_alternatives = []
        total_cost = 0
        
        for ingredient in ingredients:
            food_id = ingredient.get('id')
            food_name = ingredient.get('name')
            amount = ingredient.get('amount', 100)
            
            food_details = mcp_client.get_food_by_id(food_id)
            
            if food_details:
                nutrition_100g = food_details.get('nutrition_100g', {})
                nutritional_data = {
                    'protein': nutrition_100g.get('protein', 0),
                    'carbohydrates': nutrition_100g.get('carbohydrates', 0),
                    'fat': nutrition_100g.get('total_fat', 0),
                    'fiber': nutrition_100g.get('dietary_fiber', 0),
                    'calories': nutrition_100g.get('calories', 0)
                }
                
                price = recommendation_engine.get_mock_price(food_name)
                item_cost = (price * amount) / 100
                total_cost += item_cost
                
                catalog.add_ingredient(
                    food_id=food_id,
                    name=food_name,
                    nutritional_data=nutritional_data,
                    price=price,
                    category=food_details.get('category', 'Unknown')
                )
                
                analyzed_ingredients.append({
                    'id': food_id,
                    'name': food_name,
                    'amount': amount,
                    'nutritional_data': nutritional_data,
                    'price_per_100g': price,
                    'item_cost': round(item_cost, 2)
                })
                
                search_term = food_name.split(',')[0].split()[0]
                similar_foods = mcp_client.search_food_by_name(search_term, limit=25)
                
                protein_val = nutritional_data.get('protein', 0)
                carb_val = nutritional_data.get('carbohydrates', 0)
                calories_val = nutritional_data.get('calories', 0)
                
                if protein_val > 10:
                    protein_alternatives = mcp_client.search_food_by_name('turkey', limit=8)
                    protein_alternatives += mcp_client.search_food_by_name('tofu', limit=8)
                    protein_alternatives += mcp_client.search_food_by_name('beans', limit=8)
                    protein_alternatives += mcp_client.search_food_by_name('lentils', limit=8)
                    protein_alternatives += mcp_client.search_food_by_name('egg', limit=8)
                    protein_alternatives += mcp_client.search_food_by_name('fish', limit=8)
                    similar_foods.extend(protein_alternatives)
                
                if carb_val > 10:
                    carb_alternatives = mcp_client.search_food_by_name('rice', limit=8)
                    carb_alternatives += mcp_client.search_food_by_name('pasta', limit=8)
                    carb_alternatives += mcp_client.search_food_by_name('quinoa', limit=8)
                    carb_alternatives += mcp_client.search_food_by_name('oats', limit=8)
                    carb_alternatives += mcp_client.search_food_by_name('bread', limit=8)
                    carb_alternatives += mcp_client.search_food_by_name('potato', limit=8)
                    similar_foods.extend(carb_alternatives)
                
                if calories_val < 100:
                    veggie_alternatives = mcp_client.search_food_by_name('vegetable', limit=10)
                    veggie_alternatives += mcp_client.search_food_by_name('fruit', limit=10)
                    veggie_alternatives += mcp_client.search_food_by_name('salad', limit=10)
                    similar_foods.extend(veggie_alternatives)
                
                for similar_food in similar_foods:
                    similar_id = similar_food.get('id')
                    similar_name = similar_food.get('name', '')
                    
                    if similar_id not in [f['id'] for f in all_foods_for_alternatives]:
                        similar_details = mcp_client.get_food_by_id(similar_id)
                        if similar_details:
                            similar_nutrition = similar_details.get('nutrition_100g', {})
                            similar_nutritional = {
                                'protein': similar_nutrition.get('protein', 0),
                                'carbohydrates': similar_nutrition.get('carbohydrates', 0),
                                'fat': similar_nutrition.get('total_fat', 0),
                                'fiber': similar_nutrition.get('dietary_fiber', 0),
                                'calories': similar_nutrition.get('calories', 0)
                            }
                            similar_price = recommendation_engine.get_mock_price(similar_name)
                            
                            all_foods_for_alternatives.append({
                                'id': similar_id,
                                'name': similar_name,
                                'nutritional_data': similar_nutritional,
                                'price_per_100g': similar_price
                            })
                
                all_foods_for_alternatives.append({
                    'id': food_id,
                    'name': food_name,
                    'nutritional_data': nutritional_data,
                    'price_per_100g': price
                })
        
        all_catalog_items = catalog.get_all_ingredients()
        for item in all_catalog_items:
            if item['food_id'] not in [f['id'] for f in all_foods_for_alternatives]:
                all_foods_for_alternatives.append({
                    'id': item['food_id'],
                    'name': item['name'],
                    'nutritional_data': item['nutritional_data'],
                    'price_per_100g': item['price_per_100g']
                })
        
        recommendations = []
        total_potential_savings = 0
        
        for ingredient in analyzed_ingredients:
            alternatives = recommendation_engine.find_alternatives(
                ingredient,
                all_foods_for_alternatives,
                max_alternatives=3
            )
            
            if alternatives:
                best_alternative = alternatives[0]
                total_potential_savings += best_alternative['savings'] * (ingredient['amount'] / 100)
                
                recommendations.append({
                    'original': ingredient,
                    'alternatives': alternatives
                })
        
        catalog.save_recipe(recipe_name, analyzed_ingredients, total_cost)
        
        mcp_client.close_connection()
        
        return jsonify({
            'recipe_name': recipe_name,
            'ingredients': analyzed_ingredients,
            'total_cost': round(total_cost, 2),
            'recommendations': recommendations,
            'potential_savings': round(total_potential_savings, 2)
        })
        
    except Exception as e:
        return jsonify({'error': f'Analysis failed: {str(e)}'}), 500

@app.route('/catalog')
def view_catalog():
    ingredients = catalog.get_all_ingredients()
    return jsonify({'ingredients': ingredients})

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)
