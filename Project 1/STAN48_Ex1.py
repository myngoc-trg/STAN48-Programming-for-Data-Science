import numpy as np
import datetime

class BankAccount():
    bank_name = "My madeup bank"
    #annual_interest_rate = 0.2

    def __init__(self, owner: str, account_type: str, balance=0):
        self.owner = owner
        self.account_type = account_type.lower()
        self.balance = balance
        self.annual_interest_rate = 0.02
        self.transactions = [] #to store transaction history

    def deposit(self, amount):
        if amount <= 0:
            return "Deposit amount has to be positve number!"
        
        self.balance += amount
        self.record(action_type="deposit",amount=amount)

    def withdraw(self, amount):
        if amount <= 0:
            return "Withdraw amount has to be positve number!"
        if amount > self.balance:
            return f"Not enough money remaining! Current balance: {self.balance}."
        
        self.balance -= amount
        self.record(action_type="withdraw",amount=amount) 

    def transfer(self, target_account, amount):
        if not isinstance(target_account, BankAccount):
            return "Target account must be a bank account"
        if amount <= 0:
            return "Transfer amount has to be positve number!"
        if amount > self.balance:
            return f"Not enough money remaining! Current balance: {self.balance}."
        
        target_account.deposit(amount)
        target_account.record(action_type=f"transfer from {self.owner}",amount=amount)
        self.balance -= amount
        self.record(action_type=f"transfer to {target_account}",amount=amount)

    def apply_interest(self):
        # apply interest rate on savings account
        if self.account_type == "savings":
            interest = self.balance * self.annual_interest_rate
            self.balance += interest
        
        elif self.account_type == "morgage_loan":
            interest = self.balance * (self.annual_interest_rate*2)
            self.balance += interest
        
        elif self.account_type == "private_loan":
            interest = self.balance * (self.annual_interest_rate * 5)
            self.balance += interest
    
    def record(self, action_type, amount):
        return self.transactions.append({
            'Action': action_type,
            'Amount': amount,
            'Balance': self.balance,
            'Time': datetime.datetime.now().strftime("%Y/%m/%d, %H:%M:%S")
        })
    
    def history(self):
        for action in self.transactions:
            print("-----------------------------------------------------------------------------------------")
            print(action) 
            '''for key, val in action.items():
                print(f"{key}: {val}")
'''
     
    def __str__(self):
        return f"Bank account of {self.owner} | Account type: {self.account_type} | Balance: {self.balance}."


    
        



# extended version: bank, create account, get account, total assets, list accounts,
class Bank(BankAccount):
    def __init__(self, bank_name ="My madeup bank"):
        #self.super().__init__()
        self.accounts = []
    
    def create_account(self, owner, account_type, balance=0):
        new_acc = BankAccount(owner, account_type, balance)
        self.accounts.append(new_acc)
        return new_acc
    
    def get_account(self, owner_name):
        for acc in self.accounts:
            if acc.owner == owner_name:
                return acc
        else: 
            return "Account not found"
    
    def total_assets(self):
        total = 0
        for acc in self.accounts:
            total += acc.balance
        return total
    
    def list_accounts(self):
        for acc in self.accounts:
            print(acc)