import sqlite3
import json
from typing import List, Dict, Optional
from datetime import datetime

class IngredientCatalog:
    def __init__(self, db_path: str = 'ingredients.db'):
        self.db_path = db_path
        self.init_database()
    
    def init_database(self):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS ingredients (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                food_id TEXT UNIQUE,
                name TEXT NOT NULL,
                nutritional_data TEXT,
                price_per_100g REAL,
                category TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS recipes (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT,
                ingredients TEXT,
                total_cost REAL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        conn.commit()
        conn.close()
    
    def add_ingredient(self, food_id: str, name: str, nutritional_data: Dict, price: float = None, category: str = None):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            INSERT OR REPLACE INTO ingredients (food_id, name, nutritional_data, price_per_100g, category)
            VALUES (?, ?, ?, ?, ?)
        ''', (food_id, name, json.dumps(nutritional_data), price, category))
        
        conn.commit()
        conn.close()
    
    def get_ingredient_by_id(self, food_id: str) -> Optional[Dict]:
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('SELECT * FROM ingredients WHERE food_id = ?', (food_id,))
        row = cursor.fetchone()
        conn.close()
        
        if row:
            return {
                'id': row[0],
                'food_id': row[1],
                'name': row[2],
                'nutritional_data': json.loads(row[3]) if row[3] else {},
                'price_per_100g': row[4],
                'category': row[5],
                'created_at': row[6]
            }
        return None
    
    def get_all_ingredients(self) -> List[Dict]:
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('SELECT * FROM ingredients')
        rows = cursor.fetchall()
        conn.close()
        
        ingredients = []
        for row in rows:
            ingredients.append({
                'id': row[0],
                'food_id': row[1],
                'name': row[2],
                'nutritional_data': json.loads(row[3]) if row[3] else {},
                'price_per_100g': row[4],
                'category': row[5],
                'created_at': row[6]
            })
        return ingredients
    
    def save_recipe(self, name: str, ingredients: List[Dict], total_cost: float):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            INSERT INTO recipes (name, ingredients, total_cost)
            VALUES (?, ?, ?)
        ''', (name, json.dumps(ingredients), total_cost))
        
        conn.commit()
        conn.close()
