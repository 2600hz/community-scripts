<?php

class AccountController extends BaseController {

    protected $layout = 'layouts.master';

    public function index() {
        $this->layout->content = View::make('accounts.index');
    }

}