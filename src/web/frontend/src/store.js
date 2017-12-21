import { createStore } from 'redux'
import rootReducer from './reducers'

const defaultState = {
    // datasets: [],
}

export default createStore(rootReducer, defaultState)
