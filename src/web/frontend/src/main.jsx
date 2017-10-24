import React from 'react'
import styled from 'styled-components'

import misc from './misc/misc'

import Datasets from './datasets/datasets.jsx'
import Models from './models/models.jsx'
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

class Main extends misc.FunctionalComponent {
    constructor() {
        super()
        this.state = {
            datasets: [],
            models: [],
        }
    }

    componentDidMount() {
        fetch('/api/model/')
            .then(res => res.json())
            .then(this.setVariable('models'))
            .catch(console.error)

        fetch('/api/dataset/')
            .then(res => res.json())
            .then(this.setVariable('datasets'))
            .catch(console.error)
    }

    render() {
        return (
            <MainColumn>
                <Datasets
                    datasets={this.state.datasets}
                    uploadDataset={uploadDataset(this.update(addDataset))}
                    deleteDataset={deleteDataset(res => this.setVariable('datasets')(res.remaining))}
                />
                <Train
                    datasets={this.state.datasets}
                    train={train(this.update(addModel))}
                />
                <Models
                    models={this.state.models}
                    evaluate={evaluate}
                    deleteModel={deleteModel(res => this.setVariable('models')(res.remaining))}
                />
            </MainColumn>
        )
    }
}

export default Main
