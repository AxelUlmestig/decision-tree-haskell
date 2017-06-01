
const deleteModel = cb => name =>
    fetch('/api/model/' + name, {
        method: 'DELETE'
    })
    .then(res => res.json())
    .then(cb)
    .catch(console.log)

export default deleteModel;
