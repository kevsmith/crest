## CREST: (Concurrency | Coordination) + REST
CREST is an experiment to see what happens when concepts like [CAS](http://en.wikipedia.org/wiki/Compare-and-swap) and
[barriers](http://bit.ly/Pk6UaQ) are exposed in a RESTful
interface.

Details about the API are given below. All POST and PUT bodies MUST be valid JSON.

### Barriers

1. Create a barrier
````
POST /barriers/<name>
{"count": 1, "recycle": true}
````
Returns 204 on creation, 409 if barrier already exists

2. Wait on a barrier
````
GET /barriers/<name>?t=30000
````
"t" is timeout in milliseconds and is optional. By default all clients will wait
10 seconds if "t" is omitted.
Returns 204 when all expected parties have successfully joined.
Returns 408 if wait times out or another waiting party "hangs up".

### CAS Values

1. Create a CAS value
````
POST /values/<name>
{"value": 100}
````
NOTE: Values can be integers, floats, or strings
Returns 200 on creation, 409 if value already exists.

2. Update a CAS value
````
PUT /values/<name>
````
Supports several different update operations based on the JSON body.
+ Update to literal value
````
{"action": "write", "value": 100}
````
+ Atomic increment
````
{"action": "incr", "value": 1}
````
+ Atomic decrement
````
{"action": "decr", "value": 1}
````
All return 200 on successful update.

#### CAS Values and Preconditions
PUT and POST requests can optionally evaluate preconditions before updating the target variable.
Preconditions are submitted in a request header named `X-Crest-Expects`. Multiple preconditions
must be separated by a semicolon. If a precondition fails then the server returns a 412 status
code. Here are a few examples:

````
X-Crest-Expects: user_count
````
Succeeds only if the CAS value named `user_count` exists.

````
X-Crest-Expects: user_count > 5
````
Succeeds only if `user_count` is greater than 5.

````
X-Crest-Expects: web_servers == 8;db_ready == 'true'
````
Succeeds only if `web_servers` equals 8 and db_ready is equal to the string 'true'.

Preconditions understand the following comparison operators: <,>,==, >=, =<. Referring
to a variable without an operator checks for it's existence.