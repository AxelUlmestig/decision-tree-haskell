
const train = cb => (dataset, target) =>
    fetch('/api/dataset/' + dataset + '/train', {
        method: 'POST',
        body: JSON.stringify({
            targetVariable: target,
            significanceLevel: 0.005,
            entropyLimit: 0.2
        })
    })
    .then(res => res.json())
    .then(cb)
    .catch(console.log)

export default train;
