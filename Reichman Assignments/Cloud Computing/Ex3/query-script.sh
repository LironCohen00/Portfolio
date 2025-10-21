while IFS= read -r line; do
    postResponse=$(curl --location --request POST "http://127.0.0.1:8000/dishes" \
                --header "Content-Type: application/json" \
                --data "{\"name\": \"$line\"}")
    EncodedLine=$(echo "$line" | sed 's/ /%20/g')
    response=$(curl --location --request GET "http://127.0.0.1:8000/dishes/$EncodedLine")
    cal=$(echo "$response" | grep -o "\"cal\": [^,]*" | cut -d":" -f2 | tr -d ' ')
    sodium=$(echo "$response" | grep -o "\"sodium\": [^,]*" | cut -d":" -f2 | tr -d ' ')
    sugar=$(echo "$response" | grep -o "\"sugar\": .*" | cut -d":" -f2 | tr -d ' ')
    output="$line contains $cal calories, $sodium mgs of sodium, and $sugar grams of sugar"
    echo "$output" >> response.txt
done < queries.txt
