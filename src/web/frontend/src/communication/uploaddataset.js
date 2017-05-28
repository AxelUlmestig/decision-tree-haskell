
const uploadDataset = cb => (dataset, name) =>
    fetch('api/dataset/' + name, {
        method: 'PUT',
        body: JSON.stringify({dataset})
    })
    .then(res => res.json())
    .then(cb)
    .catch(console.log)

export default uploadDataset;
