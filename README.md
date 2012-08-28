## CREST: (Concurrency | Coordination) + REST
CREST is an experiment to see what happens when concepts like CAS (atomic compare-and-swap) and
[barriers](http://en.wikipedia.org/wiki/Barrier_(computer_science)) are exposed in a RESTful
interface.

Details about the API are given below. All POST and PUT bodies MUST be valid JSON.

### Barriers

1. Create a barrier
> POST /barriers/<name>
> Body: {"count": 1, "recycle": true}
> Returns 204 on creation, 409 if barrier already exists

2. Wait on a barrier
> GET /barriers/<name>?t=30000
> Body: NOne
> Returns 204 when all expected parties have successfully joined.
> Returns 408 if wait times out or another waiting party "hangs up".

### CAS Values

1. Create a CAS value
> POST /values/<name>
> Body: {"value": 100} (values can be integers, floats, or strings)
> Returns 200 on creation, 409 if value already exists.

2. Update a CAS value
    PUT /values/<name>
Supports several different update operations based on the JSON body.
+ Update to literal value
    {"action": "write", "value": 100}
+ Atomic increment
    {"action": "incr", "value": 1}
+ Atomic decrement
    {"action": "decr", "value": 1}
All return 200 on successful update.

3. Conditionally updating a CAS value
    PUT /values/<name>
Uses the same bodies as above with an additional preconditions hash:
    {"action": "decr", "value": 1,
    "preconditions": [{"test": 100}]}
Write operation occurs if all preconditions are met. Returns 200.
If a precondition fails, returns 412.