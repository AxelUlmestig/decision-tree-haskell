import React from 'react'
import styled from 'styled-components'
import R from 'ramda'

import Datasets from './datasets/datasets'
import Models from './models/models'
import Train from './train/train'

import uploadDataset from './communication/uploaddataset'
import deleteDataset from './communication/deletedataset'
import evaluate from './communication/evaluate'
import train from './communication/train'
import deleteModel from './communication/deletemodel'

const addModel = model => state =>
    ({
        models: state.models.concat([model]),
    })

const addDataset = dataset => state =>
    ({
        datasets: state.datasets.concat([dataset]),
    })

const MainColumn = styled.div`
    box-sizing: border-box;
    max-width: 100%;
    max-width: 600px;
    display: block;
    margin-left: auto;
    margin-right: auto;
`

class Main extends React.Component {
    constructor() {
        super()
        this.state = {
            datasets: [],
            models: [],
        }

        this.setState = this.setState.bind(this)

        this.uploadDataset = uploadDataset(R.compose(this.setState, addDataset))
        this.deleteDataset = deleteDataset(R.compose(this.setState, R.assoc('datasets'), R.path(['remaining'])))
        this.train = train(R.compose(this.setState, addModel))
        this.deleteModel = deleteModel(R.compose(this.setState, R.assoc('models'), R.path(['remaining'])))
    }

    componentDidMount() {
        fetch('/api/model/')
            .then(res => res.json())
            .then(R.compose(this.setState, R.assoc('models')))
            .catch(console.error)

        fetch('/api/dataset/')
            .then(res => res.json())
            .then(R.compose(this.setState, R.assoc('datasets')))
            .catch(console.error)
    }

    render() {
        return (
            <MainColumn>
                <Datasets
                    datasets={this.state.datasets}
                    uploadDataset={this.uploadDataset}
                    deleteDataset={this.deleteDataset}
                />
                <Train
                    datasets={this.state.datasets}
                    train={this.train}
                />
                <Models
                    models={this.state.models}
                    evaluate={evaluate}
                    deleteModel={this.deleteModel}
                />
            </MainColumn>
        )
    }
}

export default Main
