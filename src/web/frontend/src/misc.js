import React from 'react';

const getEventValue = (f, type) => event => type == 'number' ? f(parseFloat(event.target.value)) : f(event.target.value)

const id = x => x

class FunctionalComponent extends React.Component {

    constructor() {
        super();
        this.update = this.update.bind(this);
        this.setVariable = this.setVariable.bind(this);
    }

    update(f) {
        return x => this.setState(f(x, this.state));
    }

    setVariable(variable) {
        return this.update(value => state => ({[variable]: value}));
    }

}

export default {
    FunctionalComponent,
    getEventValue,
    id
}
