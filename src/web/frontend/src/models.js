import React from 'react';
import misc from './misc';

const iterateModels = (models, evaluate, deleteModel) =>
    models.map(model =>
        <li key={model.name}>
            <Model model={model} evaluate={evaluate} deleteModel={deleteModel} />
        </li>
    )

const iterateParameters = (parameters, updateParam) =>
        Object.keys(parameters).map(param => (
            <li key={param}>
                {param + ': '}
                <input
                    type={parameters[param].type}
                    value={parameters[param].value}
                    onChange={misc.getEventValue(updateParam(param), parameters[param].type)}
                />
            </li>
        ));

const updateParameter = param => value => state =>
    {
        const newState = {parameters: state.parameters};
        newState.parameters[param].value = value;
        return newState;
    }

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
        return (
            <div>
                {model.name} <button onClick={() => this.props.deleteModel(model.name)}>Delete</button>
                <ul>
                    {iterateParameters(
                            this.state.parameters,
                            param => this.update(updateParameter(param))
                    )}
                </ul>
                <button onClick={this.evaluate}>Evaluate</button>
                <div>Result: {this.state.result}</div>
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
        <b>Models</b>
        <ul>{iterateModels(props.models, props.evaluate, props.deleteModel)}</ul>
    </div>
