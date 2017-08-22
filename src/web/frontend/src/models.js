import React from 'react';
import styled from 'styled-components';

import misc from './misc';
import SectionHeader from './sectionheader.js';
import Button from './button.js';
import ItemHeader from './itemheader.js';
import Rounded from './rounded.js';

const updateParameter = param => value => state =>
    {
        const newState = {parameters: state.parameters};
        newState.parameters[param].value = value;
        return newState;
    }

const FloatRightInput = styled.input`
    float: right;
`

const ModelParameter = props =>
    <Rounded key={props.name}>
        {props.name + ': '}
        <FloatRightInput
            type={props.type}
            value={props.value}
            onChange={props.onChange || (x => x)}
            disabled={props.disabled || ''}
        />
    </Rounded>

const HorizontallyPadded = styled.div`
    padding-left: 2rem;
    padding-right: 2rem;
`

const dataTypes = {
    'NUMBER': 'number',
    'STRING': 'text'
}

const defaultValues = {
    'NUMBER': 0,
    'STRING': ''
}

class Model extends misc.FunctionalComponent {

    constructor(props) {
        super();

        const parameters = Object.keys(props.model.metaData)
            .map(x => ({
                [x]: {
                    value: defaultValues[props.model.metaData[x]],
                    type: dataTypes[props.model.metaData[x]]
                }
            }))
            .reduce((o, k) => Object.assign(o, k), {});

        this.state = {
            parameters: parameters,
            result: ''
        }

        this.evaluate = this.evaluate.bind(this);
    }

    render() {
        const model = this.props.model;
        const parameters = this.state.parameters;
        return (
            <div>
                <ItemHeader
                    text={model.name}
                    close={() => this.props.deleteModel(model.name)}>
                </ItemHeader>
                <HorizontallyPadded>
                    {Object.keys(parameters).map(param =>
                        <ModelParameter
                            name={param}
                            type={parameters[param].type}
                            value={parameters[param].value}
                            onChange={misc.getEventValue(
                                this.update(updateParameter(param)),
                                parameters[param].type
                            )}>
                        </ModelParameter>
                    )}
                    <ModelParameter
                        name={this.props.model.target}
                        type={'text'}
                        value={this.state.result}
                        disabled={'disabled'}>
                    </ModelParameter>
                    <Button label='Evaluate' action={this.evaluate}></Button>
                </HorizontallyPadded>
            </div>
        )
    }

    evaluate() {
        const cb = result => this.setState(state => ({result}))
        const name = this.props.model.name;
        const rawParameters = this.state.parameters;
        const parameters = Object.keys(rawParameters).reduce((obj, key) => {
            obj[key] = rawParameters[key].value;
            return obj;
        }, {});
        this.props.evaluate(name, parameters, cb);
    }

}

export default props =>
    <div>
        <SectionHeader value='Models'></SectionHeader>
        {props.models.map(model =>
            <Rounded key={model.name}>
                <Model model={model} evaluate={props.evaluate} deleteModel={props.deleteModel} />
            </Rounded>
        )}
    </div>
