const ADD = 'add dataset'
const OVERWRITE = 'overwrite datasets'

export const addDataset = dataset =>
    ({
        type: ADD,
        dataset,
    })

export const overwriteDatasets = datasets =>
    ({
        type: OVERWRITE,
        datasets,
    })
