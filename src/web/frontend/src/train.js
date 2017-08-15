import React from 'react';
import misc from './misc.js';

const renderOptions = (f, datasets) =>
    datasets.map((dataset, i) =>
        <option key={f(dataset)} value={i}>{f(dataset)}</option>
    )

const handleVarChange = misc.getEventValue(
        i => state =>
            ({
                selectedVar: Object.keys(state.selectedDataset.parameters)[i]
            })
    )

const handleDatasetChange = misc.getEventValue(
    i => (state, props) => {
        const selectedDataset = props.datasets[i];
        const selectedVar = Object.keys(selectedDataset.parameters)[0] || '';
        return {
            selectedDataset: selectedDataset,
            selectedVar: selectedVar
        };
    }
)

const train = () => (state, props) => {
   const selectedDataset = state.selectedDataset;
   const selectedVar = state.selectedVar;
   if(selectedDataset && selectedVar) {
       props.train(selectedDataset.name, selectedVar);
   }
   return state;
}

export default class Train extends misc.FunctionalComponent {

    constructor(props) {
        super(props);
        const emptyDataset = {name: '', parameters: []};
        const selectedDataset = props.datasets[0] || emptyDataset;
        const selectedVar = Object.keys(selectedDataset.parameters)[0] || '';
        this.state = {
            selectedDataset: selectedDataset,
            selectedVar: selectedVar
        };
    }

    componentWillReceiveProps(props) {
        this.setState(_ => ({
            selectedDataset: props.datasets[0],
            selectedVar: Object.keys(props.datasets[0].parameters)[0]
        }));
    }

    render() {
        return (
            <div>
                <div className="headerWrapper">
                    <div className="header">Train on datasets</div>
                </div>
                <br/>
                <div className="rounded">
                    <form>
                        Choose dataset to train on:
                        <select value={this.selectedDataset} onChange={this.update(handleDatasetChange)}>
                            {renderOptions(d => d.name, this.props.datasets)}
                        </select><br/>
                        Choose variable to train on:
                        <select value={this.selectedVar} onChange={this.update(handleVarChange)}>
                            {renderOptions(v => v, Object.keys(this.state.selectedDataset.parameters))}
                        </select>
                    </form><br/>
                    <button onClick={this.update(train)}>Train</button>
                </div>
            </div>
        )
    }

}
