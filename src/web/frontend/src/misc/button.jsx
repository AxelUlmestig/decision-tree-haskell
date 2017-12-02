import React from 'react'
import styled from 'styled-components'
import PropTypes from 'prop-types'

import Rounded from './rounded'

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

Button.propTypes = {
    action: PropTypes.func.isRequired,
    label: PropTypes.string.isRequired,
}

export default Button
