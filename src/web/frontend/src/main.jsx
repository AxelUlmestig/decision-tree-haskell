import React from 'react'
import PropTypes from 'prop-types'
import { Provider, connect } from 'react-redux'
import styled from 'styled-components'
import R from 'ramda'

import Datasets from './datasets/datasets'
import Models from './models/models'
import Train from './train/train'

import Store from './store'
import { addDataset, overwriteDatasets } from './actions/datasets'
import { addModel, overwriteModels } from './actions/models'
import uploadDataset from './communication/uploaddataset'
import deleteDataset from './communication/deletedataset'
import evaluate from './communication/evaluate'
import train from './communication/train'
import deleteModel from './communication/deletemodel'

const mapStateToProps = state =>
    ({
        datasets: state.datasets,
        models: state.models,
    })

const mapDispatchToProps = dispatch =>
    ({
        setDatasets: R.compose(dispatch, overwriteDatasets),
        addDataset: R.compose(dispatch, addDataset),
        setModels: R.compose(dispatch, overwriteModels),
        addModel: R.compose(dispatch, addModel),
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
    constructor(props) {
        super()

        this.uploadDataset = uploadDataset(props.addDataset)
        this.deleteDataset = deleteDataset(R.compose(props.setDatasets, R.path(['remaining'])))
        this.train = train(props.addModel)
        this.deleteModel = deleteModel(R.compose(props.setModels, R.path(['remaining'])))
    }

    componentDidMount() {
        fetch('/api/model/')
            .then(res => res.json())
            .then(this.props.setModels)
            .catch(console.error)

        fetch('/api/dataset/')
            .then(res => res.json())
            .then(this.props.setDatasets)
            .catch(console.error)
    }

    render() {
        return (
            <MainColumn>
                <Datasets
                    datasets={this.props.datasets}
                    uploadDataset={this.uploadDataset}
                    deleteDataset={this.deleteDataset}
                />
                <Train
                    datasets={this.props.datasets}
                    train={this.train}
                />
                <Models
                    models={this.props.models}
                    evaluate={evaluate}
                    deleteModel={this.deleteModel}
                />
            </MainColumn>
        )
    }
}

Main.propTypes = {
    datasets: PropTypes.arrayOf(PropTypes.shape({
        content: PropTypes.array,
        name: PropTypes.string,
        parameters: PropTypes.object,
    })).isRequired,
    setDatasets: PropTypes.func.isRequired,
    addDataset: PropTypes.func.isRequired,
    models: PropTypes.arrayOf(PropTypes.shape({
        name: PropTypes.string,
        trainingParameters: PropTypes.object,
        parameters: PropTypes.object,
        metaData: PropTypes.object,
        model: PropTypes.object,
    })).isRequired,
    addModel: PropTypes.func.isRequired,
    setModels: PropTypes.func.isRequired,
}

const ConnectedMain = connect(mapStateToProps, mapDispatchToProps)(Main)

export default () =>
    (
        <Provider store={Store}>
            <ConnectedMain />
        </Provider>
    )
