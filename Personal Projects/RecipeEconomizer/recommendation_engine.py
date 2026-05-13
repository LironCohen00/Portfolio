import random
from typing import List, Dict, Optional

class RecommendationEngine:
    
    MOCK_PRICES = {
        'chicken': 8.99,
        'beef': 12.99,
        'pork': 7.99,
        'turkey': 6.99,
        'tofu': 3.99,
        'beans': 1.99,
        'lentils': 2.49,
        'rice': 2.99,
        'pasta': 2.49,
        'bread': 3.49,
        'quinoa': 4.99,
        'oats': 2.99,
        'milk': 4.99,
        'cheese': 7.99,
        'eggs': 5.99,
        'butter': 6.49,
        'olive oil': 9.99,
        'vegetable oil': 4.99,
        'tomato': 3.99,
        'onion': 2.49,
        'garlic': 4.99,
        'potato': 1.99,
        'carrot': 2.49,
        'broccoli': 4.49,
        'spinach': 3.99,
        'lettuce': 2.99,
        'apple': 4.49,
        'banana': 1.99,
        'orange': 3.99,
        'strawberry': 5.99,
        'flour': 3.49,
        'sugar': 2.99,
        'salt': 1.49,
        'pepper': 5.99
    }
    
    def __init__(self):
        pass
    
    def get_mock_price(self, ingredient_name: str) -> float:
        ingredient_lower = ingredient_name.lower()
        
        for key in self.MOCK_PRICES:
            if key in ingredient_lower:
                base_price = self.MOCK_PRICES[key]
                variation = random.uniform(-2.0, 2.0)
                return round(max(0.99, base_price + variation), 2)
        
        random.seed(hash(ingredient_name) % 10000)
        price = round(random.uniform(2.99, 9.99), 2)
        random.seed()
        return price
    
    def calculate_nutritional_similarity(self, food1: Dict, food2: Dict) -> float:
        nutrients = ['protein', 'carbohydrates', 'fat', 'fiber', 'calories']
        
        similarity_score = 0
        nutrient_count = 0
        
        for nutrient in nutrients:
            val1 = food1.get(nutrient, 0)
            val2 = food2.get(nutrient, 0)
            
            if val1 == 0 and val2 == 0:
                continue
            
            if val1 == 0 or val2 == 0:
                similarity_score += 0
            else:
                diff = abs(val1 - val2)
                avg = (val1 + val2) / 2
                similarity = 1 - (diff / avg) if avg > 0 else 0
                similarity_score += max(0, similarity)
            
            nutrient_count += 1
        
        if nutrient_count == 0:
            return 0.5
        
        return similarity_score / nutrient_count
    
    def find_alternatives(self, ingredient: Dict, all_foods: List[Dict], max_alternatives: int = 3) -> List[Dict]:
        current_price = ingredient.get('price_per_100g') or self.get_mock_price(ingredient.get('name', ''))
        
        alternatives = []
        
        for food in all_foods:
            if food.get('id') == ingredient.get('id'):
                continue
            
            food_price = food.get('price_per_100g') or self.get_mock_price(food.get('name', ''))
            
            if food_price >= current_price:
                continue
            
            nutritional_data = food.get('nutritional_data', {})
            ingredient_nutritional = ingredient.get('nutritional_data', {})
            
            similarity = self.calculate_nutritional_similarity(ingredient_nutritional, nutritional_data)
            
            if similarity > 0.45:
                price_savings = current_price - food_price
                savings_percent = (price_savings / current_price) * 100 if current_price > 0 else 0
                
                alternatives.append({
                    'food': food,
                    'original_price': current_price,
                    'alternative_price': food_price,
                    'savings': price_savings,
                    'savings_percent': savings_percent,
                    'nutritional_similarity': similarity
                })
        
        alternatives.sort(key=lambda x: (x['savings'], x['nutritional_similarity']), reverse=True)
        
        return alternatives[:max_alternatives]
