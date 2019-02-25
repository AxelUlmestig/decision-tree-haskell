# Decision Tree

### Introduction
This is a project for training [decision trees](https://en.wikipedia.org/wiki/Decision_tree_learning).
The trees will be trained on JSON data that could look like this:
`[ { "name": "alice", "cool": true}, { "name": "bob", "cool": false } ]`

With this data you could train a model that thinks you're cool if your name is
"alice" and uncool if it's not. (Actually not because it tries to avoid
overfitting and the sample size is too small, but I hope the point gets
across).

The repo does, by default, contain a subset of the Titanic survivors dataset
which can be used to train models. Trained models are stored in the `models/`
directory. They are stored as JSON files with a recursive structure of
questions with affirmative and negative answers containing subtrees of either
more questions or an answer.

The models can then be queried by supplying data that has the same shape as the
training data. I.e. if you've got a model that predicts survival among Titanic
passengers and you've got some data of a passenger then the model can predict
if your passenger survived or not.

This project contains a library for training models on data, a HTTP API
exposing the library and a small html frontend.

### Run
To build and this project you need to have
[stack](https://docs.haskellstack.org/en/stable/README/) and
[npm](https://www.npmjs.com/get-npm) installed.

To build: \
`$ stack build` \
`$ ./build-frontend.sh`

To run: \
`$ stack exec server`

This will host a simple html page at `localhost:3000`.

### Docker Setup
Create image: \
`$ sudo docker build -t decisiontree . --rm` \
(This takes a _long_ time unfortunately)

Run image: \
`$ sudo docker run -d -p 3000:3000 decisiontree`

### Performance Test
To run performance test: \
`$ stack exec -- performance-test +RTS -A200M -N -s -l`

There is currently some parallelism this project. Figuring out which questions
to ask at each node in the training is done in parallel, this done give some
performance benefits on my 2-core machine. This is done in the function
`bestFilter` in `src/core/Train.hs`.

I've also tried to parallelize the branching during the training so that
training on different branches is not done in sequence (in the function
`constructTree` in `src/core/Train.hs`). This doesn't quite seem to work though
as the performance test doesn't speed up .

