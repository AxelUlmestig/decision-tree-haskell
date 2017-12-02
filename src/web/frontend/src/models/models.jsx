import React from 'react'
import styled from 'styled-components'
import PropTypes from 'prop-types'
import R from 'ramda'

import misc from '../misc/misc'
import SectionHeader from '../misc/sectionheader'
import Button from '../misc/button'
import ItemHeader from '../misc/itemheader'
import Rounded from '../misc/rounded'
import ModelParameter from './modelparameter'

const updateParameter = param => value => (state) => {
    const newState = { parameters: state.parameters }
    newState.parameters[param].value = value
    return newState
}

const HorizontallyPadded = styled.div`
    padding-left: 2rem;
    padding-right: 2rem;
`

const dataTypes = {
    NUMBER: 'number',
    STRING: 'text',
}

const defaultValues = {
    NUMBER: 0,
    STRING: '',
}

class Model extends React.Component {
    constructor(props) {
        super()

        const parameters = Object.keys(props.model.metaData)
            .map(x => ({
                [x]: {
                    value: defaultValues[props.model.metaData[x]],
                    type: dataTypes[props.model.metaData[x]],
                },
            }))
            .reduce((o, k) => Object.assign(o, k), {})

        this.state = {
            parameters,
            result: '',
        }

        this.setState = this.setState.bind(this)
        this.evaluate = this.evaluate.bind(this)
    }

    evaluate() {
        const cb = result => this.setState(() => ({ result }))
        const { name } = this.props.model
        const rawParameters = this.state.parameters
        const parameters = Object.keys(rawParameters).reduce(
            (obj, key) => ({ ...obj, [key]: rawParameters[key].value }),
            {},
        )
        this.props.evaluate(name, parameters, cb)
    }

    render() {
        const { model } = this.props
        const { parameters } = this.state
        return (
            <div>
                <ItemHeader
                    text={model.name}
                    close={() => this.props.deleteModel(model.name)}
                />
                <HorizontallyPadded>
                    {Object.keys(parameters).map(param =>
                        (<ModelParameter
                            name={param}
                            type={parameters[param].type}
                            value={parameters[param].value}
                            onChange={misc.getEventValue(
                                R.compose(
                                    this.setState,
                                    updateParameter(param),
                                ),
                                parameters[param].type,
                            )}
                        />))}
                    <ModelParameter
                        name={this.props.model.trainingParameters.targetVariable}
                        type="text"
                        value={this.state.result}
                        disabled="disabled"
                    />
                    <Button label="Evaluate" action={this.evaluate} />
                </HorizontallyPadded>
            </div>
        )
    }
}

Model.propTypes = {
    model: PropTypes.shape({
        metaData: PropTypes.object,
        name: PropTypes.string,
        trainingParameters: PropTypes.shape({
            targetVariable: PropTypes.string,
        }),
    }).isRequired,
    evaluate: PropTypes.func.isRequired,
    deleteModel: PropTypes.func.isRequired,
}

const Models = props => (
    <div>
        <SectionHeader value="Models" />
        {props.models.map(model => (
            <Rounded key={model.name}>
                <Model model={model} evaluate={props.evaluate} deleteModel={props.deleteModel} />
            </Rounded>))
        }
    </div>
)

Models.propTypes = {
    models: PropTypes.arrayOf(PropTypes.shape({
        name: PropTypes.string,
    })).isRequired,
    evaluate: PropTypes.func.isRequired,
    deleteModel: PropTypes.func.isRequired,
}

export default Models
