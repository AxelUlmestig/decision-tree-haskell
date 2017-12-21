import { combineReducers } from 'redux'
import R from 'ramda'

import datasets from './datasets'
import models from './models'

const rootReducer = combineReducers({
    datasets,
    models,
})

export default R.curry(rootReducer)
