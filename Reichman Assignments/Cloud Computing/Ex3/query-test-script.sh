curl --location --request POST "http://127.0.0.1:8000/dishes" \
                --header "Content-Type: application/json" \
                --data "{\"name\": \"corn flakes\"}"
response=$(curl --location --request GET "http://127.0.0.1:8000/dishes/corn%20flakes")
echo "$response" >> response.txt
