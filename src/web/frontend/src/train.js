import React from 'react';
import styled from 'styled-components';

import misc from './misc.js';
import SectionHeader from './sectionheader.js';
import Button from './button.js';
import Rounded from './rounded.js';

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

const Padded = styled.div`
    padding-top: 0.1rem;
    padding-bottom: 0.1rem;
`

const RightJustifiedSelect = styled.select`
    float: right;
`

const VariableSelect = props => {

    const options = props.options.map((option, i) =>
        <option
            key={props.displayOption(option)}
            value={i}>
            {props.displayOption(option)}
        </option>
    )

    return <Padded>
        {props.text}
        <RightJustifiedSelect
            value={props.selectedDataset}
            onChange={props.onChange}>
            {options}
        </RightJustifiedSelect>
    </Padded>

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
        this.setState(() => ({
            selectedDataset: props.datasets[0],
            selectedVar: Object.keys(props.datasets[0].parameters)[0]
        }));
    }

    render() {
        return (
            <div>
                <SectionHeader value='Train on datasets'></SectionHeader>
                <Rounded>
                    <VariableSelect
                        text='Choose dataset to train on:'
                        options={this.props.datasets}
                        displayOption={dataset => dataset.name}
                        value={this.selectedDataset}
                        onChange={this.update(handleDatasetChange)}>
                    </VariableSelect>
                    <VariableSelect
                        text='Choose variable to train on:'
                        options={Object.keys(this.state.selectedDataset.parameters)}
                        displayOption={param => param}
                        value={this.selectedVar}
                        onChange={this.update(handleVarChange)}>
                    </VariableSelect>
                </Rounded>
                <Button label='Train' action={this.update(train)}></Button>
            </div>
        )
    }

}
