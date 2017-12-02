import React from 'react'
import styled from 'styled-components'
import PropTypes from 'prop-types'
import R from 'ramda'

import Rounded from '../misc/rounded'

const FloatRightInput = styled.input`
    float: right;
`

const ModelParameter = props => (
    <Rounded key={props.name}>
        {`${props.name}: `}
        <FloatRightInput
            type={props.type}
            value={props.value}
            onChange={props.onChange || (x => x)}
            disabled={props.disabled || ''}
        />
    </Rounded>
)

ModelParameter.defaultProps = {
    onChange: R.identity,
    disabled: '',
}

ModelParameter.propTypes = {
    name: PropTypes.string.isRequired,
    type: PropTypes.string.isRequired,
    value: PropTypes.string.isRequired,
    onChange: PropTypes.func,
    disabled: PropTypes.string,
}

export default ModelParameter
