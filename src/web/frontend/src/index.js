import React from 'react';
import ReactDOM from 'react-dom';

import misc from './misc.js';

import Datasets from './datasets.js';
import Models from './models.js';
import Train from './train.js';

import uploadDataset from './communication/uploaddataset.js';
import evaluate from './communication/evaluate.js';
import train from './communication/train.js';

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
            <div>
                <Datasets
                    datasets={this.state.datasets}
                    uploadDataset={uploadDataset(this.update(addDataset))}
                    deleteDataset={console.log}
                />
                <Train
                    datasets={this.state.datasets}
                    train={train(this.update(addModel))}
                />
                <Models
                    models={this.state.models}
                    evaluate={evaluate}
                    deleteModel={console.log}
                />
            </div>
        );
    }

}

ReactDOM.render(<Main />, document.getElementById("root"));
