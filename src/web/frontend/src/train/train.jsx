import React from 'react'

import misc from '../misc/misc.js'
import SectionHeader from '../misc/sectionheader.jsx'
import Button from '../misc/button.jsx'
import Rounded from '../misc/rounded.jsx'
import VariableSelect from './variableselect.jsx'

const handleVarChange = misc.getEventValue(i => state =>
    ({
        selectedVar: Object.keys(state.selectedDataset.parameters)[i],
    }))

const handleDatasetChange = misc.getEventValue(i => (state, props) => {
    const selectedDataset = props.datasets[i]
    const selectedVar = Object.keys(selectedDataset.parameters)[0] || ''
    return {
        selectedDataset,
        selectedVar,
    }
})

const train = () => (state, props) => {
    const selectedDataset = state.selectedDataset
    const selectedVar = state.selectedVar
    if (selectedDataset && selectedVar) {
        props.train(selectedDataset.name, selectedVar)
    }
    return state
}

export default class Train extends misc.FunctionalComponent {
    constructor(props) {
        super(props)
        const emptyDataset = { name: '', parameters: [] }
        const selectedDataset = props.datasets[0] || emptyDataset
        const selectedVar = Object.keys(selectedDataset.parameters)[0] || ''
        this.state = {
            selectedDataset,
            selectedVar,
        }
    }

    componentWillReceiveProps(props) {
        this.setState(() => ({
            selectedDataset: props.datasets[0],
            selectedVar: Object.keys(props.datasets[0].parameters)[0],
        }))
    }

    render() {
        return (
            <div>
                <SectionHeader value="Train on datasets" />
                <Rounded>
                    <VariableSelect
                        text="Choose dataset to train on:"
                        options={this.props.datasets}
                        displayOption={dataset => dataset.name}
                        value={this.selectedDataset}
                        onChange={this.update(handleDatasetChange)}
                    />
                    <VariableSelect
                        text="Choose variable to train on:"
                        options={Object.keys(this.state.selectedDataset.parameters)}
                        displayOption={param => param}
                        value={this.selectedVar}
                        onChange={this.update(handleVarChange)}
                    />
                </Rounded>
                <Button label="Train" action={this.update(train)} />
            </div>
        )
    }
}
