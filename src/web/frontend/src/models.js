import React from 'react';

const iterateModels = models =>
    models.
        map(model => <li>{model.name}</li>)

export default props =>
    <div>
        <b>Models</b>
        <ul>{iterateModels(props.models)}</ul>
    </div>
