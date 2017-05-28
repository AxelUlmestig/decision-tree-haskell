
const deleteDataset = cb => name =>
    fetch('/api/dataset/' + name, {
        method: 'DELETE'
    })
    .then(res => res.json())
    .then(cb)
    .catch(console.log)

export default deleteDataset;
