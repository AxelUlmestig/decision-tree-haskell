import React from 'react'
import PropTypes from 'prop-types'
import R from 'ramda'

import misc from '../misc/misc'
import SectionHeader from '../misc/sectionheader'
import Button from '../misc/button'
import Rounded from '../misc/rounded'
import VariableSelect from './variableselect'

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
    const { selectedDataset, selectedVar } = state
    if (selectedDataset && selectedVar) {
        props.train(selectedDataset.name, selectedVar)
    }
    return state
}

class Train extends React.Component {
    constructor(props) {
        super(props)
        const emptyDataset = { name: '', parameters: [] }
        const selectedDataset = props.datasets[0] || emptyDataset
        const selectedVar = Object.keys(selectedDataset.parameters)[0] || ''
        this.state = {
            selectedDataset,
            selectedVar, // eslint-disable-line react/no-unused-state
        }

        this.setState = this.setState.bind(this)
    }

    componentWillReceiveProps(props) {
        if (props.datasets.length === 0) return

        const firstDataset = props.datasets[0]
        const firstParameter = Object.keys(firstDataset.parameters)[0]

        this.setState(() => ({
            selectedDataset: firstDataset,
            selectedVar: firstParameter,
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
                        onChange={
                            R.compose(
                                this.setState,
                                handleDatasetChange,
                            )
                        }
                    />
                    <VariableSelect
                        text="Choose variable to train on:"
                        options={Object.keys(this.state.selectedDataset.parameters)}
                        displayOption={param => param}
                        value={this.selectedVar}
                        onChange={
                            R.compose(
                                this.setState,
                                handleVarChange,
                            )
                        }
                    />
                </Rounded>
                <Button
                    label="Train"
                    action={
                        R.compose(
                            this.setState,
                            train,
                        )
                    }
                />
            </div>
        )
    }
}

Train.propTypes = {
    datasets: PropTypes.arrayOf({
        parameters: PropTypes.object,
    }).isRequired,
}

export default Train
