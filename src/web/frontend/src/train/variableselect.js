import React from 'react';
import styled from 'styled-components';

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

export default VariableSelect
