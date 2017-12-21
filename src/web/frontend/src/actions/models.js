const ADD = 'add model'
const OVERWRITE = 'overwrite models'

export const addModel = model =>
    ({
        type: ADD,
        model,
    })

export const overwriteModels = models =>
    ({
        type: OVERWRITE,
        models,
    })
