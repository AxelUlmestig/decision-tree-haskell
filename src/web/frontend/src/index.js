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
    }

    componentDidMount() {
        fetch('/api/model/')
            .then(res => res.json())
            .then(models => this.setState({models}))
    }

    render() {
        return (
            <div>
                <Datasets />
                <Models models={this.state.models} />
            </div>
        );
    }

}

ReactDOM.render(<Main />, document.getElementById("root"));
