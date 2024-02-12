# toposoid-sat-solver-web
This is a WEB API that works as a microservice within the Toposoid project.
Toposoid is a knowledge base construction platform.(see [Toposoid　Root Project](https://github.com/toposoid/toposoid.git))
This microservice provides solver functionality for Boolean satisfiability problems (SAT) or similar problems (Max-SAT etc.).

[![Test And Build](https://github.com/toposoid/toposoid-sat-solver-web/actions/workflows/action.yml/badge.svg)](https://github.com/toposoid/toposoid-sat-solver-web/actions/workflows/action.yml)

## Requirements
* Docker version 20.10.x, or later
* docker-compose version 1.22.x

## Setup For Standalone
```bssh
docker-compose up
```

## Usage
```bash
curl -X POST -H "Content-Type: application/json" -d '{
    "regulation": {
        "formula": "1 2 OR 3 OR",
        "subFormulaMap": {
            "1": "1 2 AND 1 3 NOT AND AND",
            "2": "1 2 NOT AND 1 3 AND AND",
            "3": "1 NOT 2 AND 2 3 AND AND"
        }
    },
    "hypothesis": {
        "formula": "1 6 AND 10 OR",
        "subFormulaMap": {
            "1": "1 2 AND 3 4 OR 4 true AND AND 3 true AND AND IMP",
            "6": "true 7 AND 8 true OR IMP",
            "10": "10 11 AND 10 12 AND AND 13 14 OR IMP"
        }
    }
}' http://localhost:9009/execute
```
The requset json consists of regulation and hypothesis. In regulation, describe the tautology (which must be true).
hypothesis describes as many logical expressions as possible.
The way of describing the logical expression is that the first layer is the structure of the main expression, and the second layer describes the structure when the first layer has the logical structure as a breakdown.
This description expresses a tree structure of two-step hierarchical logical expressions. First, the target logical expression is expressed as a binary graph. 
* formula: It expresses the first-level formula in Reverse Polish Notation RPN format.　However, the only operators are AND and OR.
* subFormulaMap: This is expressed in reverse Polish notation RPN format for the second-tier logical expression, and is described in Map format with the ID that identifies the node in the first-tier as the key. However, the only operators are AND and OR and →.

## Note
* This microservice uses 9009 as the default port.
* If you want to run in a remote environment or a virtual environment, change PRIVATE_IP_ADDRESS in docker-compose.yml according to your environment.
* The SatSolver library uses [EvalMaxSAT](https://github.com/toposoid/EvalMaxSAT).　

## License
toposoid/toposoid-sat-solver-web is Open Source software released under the [Apache 2.0 license](https://www.apache.org/licenses/LICENSE-2.0.html).

## Author
* Makoto Kubodera([Linked Ideal LLC.](https://linked-ideal.com/))

Thank you!

