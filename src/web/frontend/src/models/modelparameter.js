import React from 'react';
import styled from 'styled-components';

import Rounded from '../misc/rounded.js';

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

export default ModelParameter
