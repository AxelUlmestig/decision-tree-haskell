import React from 'react';
import ReactDOM from 'react-dom';

import Datasets from './datasets.js'
import Models from './models.js'

class Main extends React.Component {

    constructor() {
        super();
        this.state = {
            datasets: [],
            models: []
        }

        fetch('http://localhost:3000/api/model/')
        .then(console.log)
        .catch(console.log)
    }

    render() {
        return (
            <h1>
                <Datasets />
                <Models />
            </h1>
        );
    }

}

ReactDOM.render(<Main />, document.getElementById("root"));
