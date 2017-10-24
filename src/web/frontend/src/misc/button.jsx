import React from 'react'
import styled from 'styled-components'

import Rounded from './rounded.jsx'

const Styled = styled.div`
    cursor: pointer;
    text-align: center;
`

const Button = props => (
    <Rounded>
        <Styled
            onClick={props.action}
        >
            {props.label}
        </Styled>
    </Rounded>
)

export default Button
