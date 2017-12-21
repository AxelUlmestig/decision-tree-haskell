import R from 'ramda'

const ADD = 'add model'
const OVERWRITE = 'overwrite models'

export default (state = [], payload) => {
    switch (payload.type) {
    case ADD: {
        return R.append(payload.model, state)
    }

    case OVERWRITE: {
        return payload.models
    }

    default: {
        return state
    }
    }
}
