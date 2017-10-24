
const evaluate = (name, params, cb) =>
    fetch(`/api/model/${name}/evaluate`, {
        method: 'POST',
        body: JSON.stringify(params),
    })
        .then(res => res.json())
        .then(res => cb(res.answer))
        .catch(console.log)

export default evaluate
