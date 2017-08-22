import React from 'react';
import styled from 'styled-components';

const Wrapper = styled.div`
    position: relative;
    overflow: hidden;
    display: flex;
    justify-content: space-between;
`

const TextContent = styled.div`
    display: flex;
    flex-direction: column;
    justify-content: center;
`

const DeleteButton = styled.img`
    height: 100%;
    height: 2rem;
    width: 100%;
    width: 2rem;
    align-self: flex-end;
    cursor: pointer;
`

const ItemHeader = props => (
    <Wrapper>
        <TextContent>{props.text}</TextContent>
        <DeleteButton
            src={require('./static/delete-button.png')}
            onClick={props.close}>
        </DeleteButton>
    </Wrapper>
)

export default ItemHeader;
