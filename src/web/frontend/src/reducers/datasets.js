import R from 'ramda'

const ADD = 'add dataset'
const OVERWRITE = 'overwrite datasets'

const initialState = []

export default (state = initialState, payload) => {
    switch (payload.type) {
    case ADD: {
        return R.append(payload.dataset, state)
    }

    case OVERWRITE: {
        return payload.datasets
    }

    default: {
        return state
    }
    }
}
