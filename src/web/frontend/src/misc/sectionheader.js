import React from 'react';
import styled from 'styled-components';

const HeaderWrapper = styled.div`
    text-align: center;
    padding-top: 0.7rem;
    padding-bottom: 0.7rem;
`

const Header = styled.div`
    font-size: 2rem;
    font-weight: bold;
`

const SectionHeader = props => (
    <HeaderWrapper>
        <Header>{props.value}</Header>
    </HeaderWrapper>
)

export default SectionHeader;
