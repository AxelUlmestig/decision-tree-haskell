import React from 'react';
import misc from './misc';

const iterateModels = (models, evaluate, deleteModel) =>
    models.map(model =>
        <div key={model.name} className='rounded'>
            <Model model={model} evaluate={evaluate} deleteModel={deleteModel} />
        </div>
    )

const iterateParameters = (parameters, updateParam) =>
        Object.keys(parameters).map(param => (
            <div key={param} className='rounded'>
                {param + ': '}
                <input
                    type={parameters[param].type}
                    value={parameters[param].value}
                    onChange={misc.getEventValue(updateParam(param), parameters[param].type)}
                />
            </div>
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
                <div className='deleteButtonWrapper'>
                    <div className='verticallyAlignedWrapper'>
                        {model.name}
                    </div>
                    <img
                        className='deleteButton clickable'
                        src={require('./static/delete-button.png')}
                        onClick={() => this.props.deleteModel(model.name)}>
                    </img>
                </div>
                <div className='horizontallyPadded'>
                    {iterateParameters(
                            this.state.parameters,
                            param => this.update(updateParameter(param))
                    )}
                    <div className='rounded'>
                        {this.props.model.target + ':'}
                        <input
                            value={this.state.result}
                            disabled='disabled'
                        />
                    </div>
                    <div onClick={this.evaluate} className='rounded clickable centeredWrapper'>Evaluate</div>
                </div>
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
        <div className="headerWrapper">
            <div className="header">Models</div>
        </div>
        {iterateModels(props.models, props.evaluate, props.deleteModel)}
    </div>
