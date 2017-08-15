import React from 'react';
import ReactDOM from 'react-dom';

import './index.css';

import misc from './misc.js';

import Datasets from './datasets.js';
import Models from './models.js';
import Train from './train.js';

import uploadDataset from './communication/uploaddataset.js';
import deleteDataset from './communication/deletedataset.js';
import evaluate from './communication/evaluate.js';
import train from './communication/train.js';
import deleteModel from './communication/deletemodel.js';

const addModel = model => state =>
    ({
        models: state.models.concat([model])
    })

const addDataset = dataset => state =>
    ({
        datasets: state.datasets.concat([dataset])
    })

class Main extends misc.FunctionalComponent {

    constructor() {
        super();
        this.state = {
            datasets: [],
            models: []
        }
    }

    componentDidMount() {
        fetch('/api/model/')
            .then(res => res.json())
            .then(this.setVariable('models'))
            .catch(console.log)

        fetch('/api/dataset/')
            .then(res => res.json())
            .then(this.setVariable('datasets'))
            .catch(console.log)
    }

    render() {
        return (
            <div className="main">
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
            </div>
        );
    }

}

ReactDOM.render(<Main />, document.getElementById("root"));
