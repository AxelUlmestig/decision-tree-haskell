
const train = cb => (dataset, target) =>
    fetch('/api/dataset/' + dataset + '/train', {
        method: 'POST',
        body: JSON.stringify({
            targetvar: target
        })
    })
    .then(res => res.json())
    .then(cb)
    .catch(console.log)

export default train;
