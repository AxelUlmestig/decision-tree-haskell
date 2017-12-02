
const getEventValue = (f, type) => event => (type === 'number' ? f(parseFloat(event.target.value)) : f(event.target.value))

export default {
    getEventValue,
}
