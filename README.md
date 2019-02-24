# Decision Tree

This is a project for training decision trees.

To build:
`$ stack build`
`$ ./build-frontend.sh`

To run:
`$ stack exec server`

This will host a simple html page at `localhost:3000` where you can upload
datasets and train trees using them. The uploaded datasets should be JSON
arrays of objects e.g.
`[ { "name": "lisa", "cool": true}, { "name": "anna", "cool": false } ]`

### Docker Setup
Create image
`$ sudo docker build -t decisiontree . --rm`
(This takes a _long_ time unfortunately)

Run image
`$ sudo docker run -d -p 3000:3000 decisiontree`

### Performance Test
To run performance test:
`$ stack exec -- performance-test +RTS -A200M -N -s -l`

There is currently some parallelism this project. Figuring out which questions
to ask at each node in the training is done in parallel, this done give some
performance benefits on my 2-core machine. This is done in the function
`bestFilter` in `src/core/Train.hs`.

I've also tried to parallelize the branching during the training so that
training on different branches is not done in sequence (in the function
`constructTree` in `src/core/Train.hs`). This doesn't quite seem to work though
as the performance test doesn't speed up .

