# gravity

## Prerequisites
- jdk8
- Reasonably modern versions of `sbt` and `node` (tested on `sbt 0.13.12` and `node 7.0.0`)

## Installation
```bash
sbt app/fastOptJS
pushd app
npm install
npm start
popd
```
and go to [http://localhost:8090/](http://localhost:8090/).
