TOKEN=$(grep BACKEND_TOKEN_SECRET .env | cut -d '=' -f2-)

wscat \
  -H "x-service-token: $TOKEN" \
  -c ws://localhost:5000/websocket
set -e

TOKEN=$(grep BACKEND_TOKEN_SECRET .env | cut -d '=' -f2-)

echo "[WS] connecting..."
(
    sleep 0.5
    echo '{"type":"SUBSCRIBE","mapId":1}'
) | wscat \
-H "x-service-token: $TOKEN" \
-c ws://localhost:5000/websocket

wscat \
-H "x-service-token: +k67H/oUCBtpg7ef8PuTPQKJm0FyPdlFZGVY3MRfULx+Tz5fYgwdEPy+GyxuH9NRkjHi7WwQG+92eLWqrF6MZJ5BnXJ9dskTyyXq3fHrQ70/L66cpvxwJA==" \
-c ws://localhost:5000/websocket

{"type":"SUBSCRIBE","target":"map-97ac4df6-e3c2-4599-9340-473b7019c372"}

wscat \
-c ws://localhost:5000/websocket

wscat \
-H "Cookie: session=dev-session-123" \
-c ws://localhost:5000/websocket

wscat \
-H "Cookie: session=dfsajfdskjafkdsjfiwjkewjmsnvdmvxccpiwopew4938493498432j" \
-c ws://localhost:5000/websocket
