import json
import subprocess
import sys
import shutil
import platform
from typing import List, Dict, Optional

class OpenNutritionMCPClient:
    def __init__(self, mcp_server_path: str):
        self.mcp_server_path = mcp_server_path
        self.process = None
        self.request_id = 0
        self.node_executable = self._find_node_executable()
    
    def _find_node_executable(self) -> str:
        node_path = shutil.which('node')
        if node_path:
            return node_path
        
        if platform.system() == 'Windows':
            node_path = shutil.which('node.exe')
            if node_path:
                return node_path
            
            print("WARNING: Node.js not found in PATH. Please install Node.js v20+ from https://nodejs.org/", file=sys.stderr)
        else:
            print("WARNING: Node.js not found in PATH. Please install Node.js v20+ or add it to your PATH.", file=sys.stderr)
        
        return 'node'
    
    def _write_message(self, message: Dict) -> None:
        if not self.process or not self.process.stdin:
            raise Exception("MCP process not started")
        
        message_json = json.dumps(message) + '\n'
        self.process.stdin.write(message_json.encode('utf-8'))
        self.process.stdin.flush()
    
    def _read_message(self) -> Dict:
        if not self.process or not self.process.stdout:
            raise Exception("MCP process not started")
        
        while True:
            line = self.process.stdout.readline()
            if not line:
                raise Exception("MCP server closed connection")
            
            line_str = line.decode('utf-8').strip()
            
            if not line_str:
                continue
            
            if line_str.startswith('{'):
                try:
                    return json.loads(line_str)
                except json.JSONDecodeError:
                    continue
            else:
                continue
    
    def _send_request(self, method: str, params: Dict) -> Dict:
        if not self.process:
            return {"error": "MCP connection not established"}
        
        try:
            self.request_id += 1
            request = {
                "jsonrpc": "2.0",
                "id": self.request_id,
                "method": method,
                "params": params
            }
            
            self._write_message(request)
            
            while True:
                response = self._read_message()
                
                if 'id' in response and response['id'] == self.request_id:
                    return response
                
                if 'method' in response:
                    continue
            
        except Exception as e:
            print(f"Error communicating with MCP server: {e}", file=sys.stderr)
            return {"error": str(e)}
    
    def start_connection(self):
        try:
            popen_kwargs = {
                'stdin': subprocess.PIPE,
                'stdout': subprocess.PIPE,
                'stderr': subprocess.PIPE,
                'bufsize': 0,
                'text': False
            }
            
            if platform.system() == 'Windows':
                CREATE_NO_WINDOW = 0x08000000
                popen_kwargs['creationflags'] = CREATE_NO_WINDOW
            
            print(f"Starting MCP server: {self.node_executable} {self.mcp_server_path}", file=sys.stderr)
            
            self.process = subprocess.Popen(
                [self.node_executable, self.mcp_server_path],
                **popen_kwargs
            )
            
            # Check if process started
            import time
            time.sleep(0.5)
            poll_result = self.process.poll()
            if poll_result is not None:
                # Process exited immediately
                stderr_output = self.process.stderr.read().decode('utf-8', errors='ignore')
                raise Exception(f"MCP server failed to start. Exit code: {poll_result}. Error: {stderr_output}")
            
            initialize_request = {
                "jsonrpc": "2.0",
                "id": 0,
                "method": "initialize",
                "params": {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {},
                    "clientInfo": {
                        "name": "recipe-analyzer",
                        "version": "1.0.0"
                    }
                }
            }
            
            self._write_message(initialize_request)
            init_response = self._read_message()
            
            if 'result' in init_response:
                initialized_notification = {
                    "jsonrpc": "2.0",
                    "method": "notifications/initialized"
                }
                self._write_message(initialized_notification)
                return True
            
            return False
            
        except Exception as e:
            print(f"Failed to start MCP connection: {e}", file=sys.stderr)
            if self.process:
                self.process.terminate()
                self.process = None
            return False
    
    def search_food_by_name(self, food_name: str, limit: int = 10) -> List[Dict]:
        response = self._send_request("tools/call", {
            "name": "search-food-by-name",
            "arguments": {
                "query": food_name,
                "limit": limit
            }
        })
        
        if 'result' in response and 'content' in response['result']:
            content = response['result']['content']
            if content and len(content) > 0:
                try:
                    return json.loads(content[0].get('text', '[]'))
                except:
                    return []
        return []
    
    def get_food_by_id(self, food_id: str) -> Optional[Dict]:
        response = self._send_request("tools/call", {
            "name": "get-food-by-id",
            "arguments": {
                "id": food_id
            }
        })
        
        if 'result' in response and 'content' in response['result']:
            content = response['result']['content']
            if content and len(content) > 0:
                try:
                    return json.loads(content[0].get('text', '{}'))
                except:
                    return None
        return None
    
    def close_connection(self):
        if self.process:
            self.process.terminate()
            self.process.wait()
